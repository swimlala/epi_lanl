CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:57Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �\   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �`   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �h   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               DA   AO  20111130144211  20190522121829  1728_5048_068                   2C  D   APEX                            2142                            040306                          846 @���_�P1   @����x@6���$��c��R1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BW��B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C��C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy�fD���D�6fD�Y�D���D���D�<�D���D��fD���D�33D�Y�D�� D��3D��D�y�D�3D�� D���D�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBV  B^ffBfffBn��BvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C�3C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C�ٚC�ٚC���C���C���C�ٚC���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC�ٚC���C���C���C���C���C���C���C�ٚC�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD��DffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!l�D!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL��DMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDml�Dm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffDy��D�� D�)�D�L�D�� D�� D�0 D�� D���D�� D�&fD�L�Dǳ3D��fD� D�l�D�fD��3D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�"�A��#A��!A���A�|�A�n�A�ffA�Q�A�/A��RA���A�7LA���A��\A�l�A�5?A��A�A���A��FA���A�M�A�JA��A���A��^A���A�r�A�K�A�5?A�/A�
=A���A�ƨA��;A��TA�ƨA���A��PA�XA�;dA�/A���A��A�1'A���A��-A��7A�ZA�bA���A���A��A�9XA�oA��yA��-A�p�A�7LA��TA���A�`BA�;dA�/A�$�A���A��A��A�^5A�Q�A��;A��uA�bNA�A��wA�\)A�$�A�
=A�A�O�A�z�A��`A��7A�A�A�r�A�;dA��A��A��A�=qA��/A���A��PA�M�A�  A�Q�A���A�$�A��-A�M�A�v�A��A��A� �A��A�hsA���A�(�A���A�M�A��A��/A��`A�G�A�A��jA��A�A�hsA��PA��`A���A�`BA��;A�VA�n�A�dZA���A�A�C�A��jA��#A���A�p�A� �A��A�O�A�K�A�jA�-A���A��\A��A��A|�/A{S�Az^5Ay;dAsp�An(�AlbAjffAi��Ai"�AhffAg��Aep�A_oA\�DAZz�AX��AW�wAV�AQ�hAOG�ANVAJ��AI/AHbNAGO�AE�#AD�\ACS�AA��A@�A>9XA=C�A;��A9G�A8�jA7��A7O�A6�A4��A3"�A0��A.�A+�;A*1'A)%A(9XA'S�A&�HA&JA$�HA$$�A#ƨA"�A �A r�A?}A��A(�A�9A��A��A�^A��AVA5?A��AK�A�yAjA��A�yAE�A�A��A%A��A5?A�7A�9A�A��A��A�A
��A
^5A
�A	oA�uA9XA1AA�A��AdZAA{AC�A ȴA =q@�C�@��H@��`@�9X@�-@�33@�@�Z@�R@��`@�S�@�=q@��m@���@�u@�P@�n�@� �@��@���@�ȴ@ف@ؓu@�$�@�bN@�7L@�Z@ϥ�@�"�@�{@��@�1@ʧ�@ɲ-@�?}@�%@�/@�@�~�@̼j@�&�@�X@�`B@�hs@��/@�A�@��
@�C�@ʰ!@�X@ȣ�@��
@��H@�J@�p�@�V@�Q�@�9X@ēu@ċD@�dZ@°!@���@�M�@��-@�O�@�`B@�&�@��@�ƨ@�"�@�E�@�V@��P@�$�@� �@��@�|�@�33@���@��\@�v�@�M�@�$�@�@�@���@�`B@��`@�Z@��m@��@�K�@�o@��!@�^5@�J@���@�hs@�%@��9@��@�Q�@��@�|�@�"�@���@�ȴ@���@�ff@���@�/@��9@��w@�dZ@�
=@��T@�Z@��@�X@���@�b@��@��@���@�`B@��D@��@�C�@�@��H@�@���@�ƨ@��@�|�@��@�bN@��@�K�@��@��R@�E�@��7@�7L@�%@���@�A�@��@���@��
@��w@���@�o@���@�^5@�5?@��@�@�@�5?@�J@��#@��^@��h@�x�@�&�@��@�O�@�hs@�p�@�hs@�p�@�p�@�hs@�p�@�X@�?}@�7L@��@��j@�bN@� �@�1@���@��@�dZ@�C�@�o@��y@���@��\@�=q@�$�@��@���@��^@��h@�x�@�hs@�?}@�V@��/@�r�@�A�@�1@�t�@�+@��H@���@���@�v�@�n�@�^5@�V@�=q@��@���@��h@�x�@��@��j@���@�Q�@��@���@��@���@�dZ@�33@�o@�@���@���@��\@�v�@�$�@���@�hs@�/@���@���@�I�@�(�@�1@��@��F@��P@��@��h@{�F@r��@mV@c�m@[33@S�F@M`B@FE�@@��@;@5�h@/|�@*�@$Z@ bN@1@|�@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�"�A��#A��!A���A�|�A�n�A�ffA�Q�A�/A��RA���A�7LA���A��\A�l�A�5?A��A�A���A��FA���A�M�A�JA��A���A��^A���A�r�A�K�A�5?A�/A�
=A���A�ƨA��;A��TA�ƨA���A��PA�XA�;dA�/A���A��A�1'A���A��-A��7A�ZA�bA���A���A��A�9XA�oA��yA��-A�p�A�7LA��TA���A�`BA�;dA�/A�$�A���A��A��A�^5A�Q�A��;A��uA�bNA�A��wA�\)A�$�A�
=A�A�O�A�z�A��`A��7A�A�A�r�A�;dA��A��A��A�=qA��/A���A��PA�M�A�  A�Q�A���A�$�A��-A�M�A�v�A��A��A� �A��A�hsA���A�(�A���A�M�A��A��/A��`A�G�A�A��jA��A�A�hsA��PA��`A���A�`BA��;A�VA�n�A�dZA���A�A�C�A��jA��#A���A�p�A� �A��A�O�A�K�A�jA�-A���A��\A��A��A|�/A{S�Az^5Ay;dAsp�An(�AlbAjffAi��Ai"�AhffAg��Aep�A_oA\�DAZz�AX��AW�wAV�AQ�hAOG�ANVAJ��AI/AHbNAGO�AE�#AD�\ACS�AA��A@�A>9XA=C�A;��A9G�A8�jA7��A7O�A6�A4��A3"�A0��A.�A+�;A*1'A)%A(9XA'S�A&�HA&JA$�HA$$�A#ƨA"�A �A r�A?}A��A(�A�9A��A��A�^A��AVA5?A��AK�A�yAjA��A�yAE�A�A��A%A��A5?A�7A�9A�A��A��A�A
��A
^5A
�A	oA�uA9XA1AA�A��AdZAA{AC�A ȴA =q@�C�@��H@��`@�9X@�-@�33@�@�Z@�R@��`@�S�@�=q@��m@���@�u@�P@�n�@� �@��@���@�ȴ@ف@ؓu@�$�@�bN@�7L@�Z@ϥ�@�"�@�{@��@�1@ʧ�@ɲ-@�?}@�%@�/@�@�~�@̼j@�&�@�X@�`B@�hs@��/@�A�@��
@�C�@ʰ!@�X@ȣ�@��
@��H@�J@�p�@�V@�Q�@�9X@ēu@ċD@�dZ@°!@���@�M�@��-@�O�@�`B@�&�@��@�ƨ@�"�@�E�@�V@��P@�$�@� �@��@�|�@�33@���@��\@�v�@�M�@�$�@�@�@���@�`B@��`@�Z@��m@��@�K�@�o@��!@�^5@�J@���@�hs@�%@��9@��@�Q�@��@�|�@�"�@���@�ȴ@���@�ff@���@�/@��9@��w@�dZ@�
=@��T@�Z@��@�X@���@�b@��@��@���@�`B@��D@��@�C�@�@��H@�@���@�ƨ@��@�|�@��@�bN@��@�K�@��@��R@�E�@��7@�7L@�%@���@�A�@��@���@��
@��w@���@�o@���@�^5@�5?@��@�@�@�5?@�J@��#@��^@��h@�x�@�&�@��@�O�@�hs@�p�@�hs@�p�@�p�@�hs@�p�@�X@�?}@�7L@��@��j@�bN@� �@�1@���@��@�dZ@�C�@�o@��y@���@��\@�=q@�$�@��@���@��^@��h@�x�@�hs@�?}@�V@��/@�r�@�A�@�1@�t�@�+@��H@���@���@�v�@�n�@�^5@�V@�=q@��@���@��h@�x�@��@��j@���@�Q�@��@���@��@���@�dZ@�33@�o@�@���@���@��\@�v�@�$�@���@�hs@�/@���@���@�I�@�(�@�1@��@��F@��P@��@��h@{�F@r��@mV@c�m@[33@S�F@M`B@FE�@@��@;@5�h@/|�@*�@$Z@ bN@1@|�@�
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBp�Bo�Bp�Bq�Br�Bs�Br�Bq�Bp�Bn�BcTBN�B,BoB�B�B(�B2-B;dBP�BW
B\)BjBx�B� B�B�+B�\B�hB�uB��B��B��B�oB�oB��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�'B�3B�LB�jBĜBɺB��B�B��B��B��B��B��B��B��B��B��B��B��BɺBǮB�^B�LB�'B�'B�'B��B��B�hB�uB�hB�PB�=B�Bp�BjB`BBW
BT�BJ�BE�B6FB0!B�B�BJBBB��B��B�B�B�TB�/B��BǮBŢB�}B�jB�FB�'B�B��B�{Bx�BgmBYBN�B:^B&�BB
�HB
�qB
�B
�oB
�=B
{�B
o�B
m�B
e`B
aHB
O�B
6FB
�B
%B
B	�B	��B	�B	�uB	�B	~�B	y�B	t�B	iyB	XB	/B	�B	uB	\B��B�B�HB��B��BÖB�RB�3B�!B��B��B��B��B��B�bB�DB�JB�7B�%B�B}�B{�Bv�By�Bs�Bl�B^5BaHBaHBcTBk�BhsBiyBk�Bn�Bq�Br�Be`Be`BffBhsBdZBbNBgmBaHBdZBaHB`BBaHB`BB`BB_;B^5B\)B\)B\)B[#B[#B[#B[#BZBYBYBXBW
BW
BYBZB[#B\)BbNBYB[#B]/B^5B\)BZB]/BbNB[#BbNBZB^5BW
B\)BYBXBR�BT�BS�BT�BQ�BW
BVBQ�BI�BD�BG�BI�BF�BC�B>wB=qB:^B=qB<jB>wB9XB7LB6FB:^B5?B49B9XB?}BM�BC�B9XB;dB;dB?}BB�BiyBp�Bs�Bt�Bv�B�B�1B�B�%B�+B�\B�+B�7B�B�B�B�%B�1B�%B�JB�bB��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�!B�9B�RB�XB�jB�}BBŢBǮB��B��B�#B�HB�ZB�B�B��B��B��B��B	  B	B	
=B	JB	JB	bB	�B	�B	�B	�B	�B	�B	�B	$�B	%�B	)�B	"�B	!�B	#�B	$�B	&�B	&�B	oB	hB	hB	\B	\B	bB	bB	hB	uB	�B	�B	�B	%�B	'�B	)�B	-B	:^B	B�B	C�B	E�B	G�B	H�B	G�B	K�B	M�B	N�B	O�B	R�B	VB	W
B	XB	[#B	^5B	cTB	e`B	jB	k�B	n�B	p�B	u�B	y�B	{�B	}�B	� B	�B	�B	�1B	�DB	�PB	�hB	�uB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�FB	�RB	�RB	�RB	�XB	�jB	�qB	�}B	�}B	��B	��B	��B	��B	B	B	B	ÖB	ĜB	ĜB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�)B	�/B	�;B	�BB	�;B	�NB	�TB	�NB	�TB	�TB	�ZB	�`B	�mB	��B
B
JB
�B
�B
)�B
.B
5?B
<jB
B�B
H�B
N�B
R�B
XB
^5B
bNB
ffB
l�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Bs�Bq�Bq�Br�Bs�Bs�Br�Bq�Bq�Bp�BgmBVB6FB{B�B�B)�B33B<jBQ�BXB^5Bk�By�B�B�B�1B�bB�oB�{B��B��B��B�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�'B�3B�RB�wBŢB��B��B�B��B��B��B�
B�B��B��B��B�
B�B��B��B��B��B�^B�3B�3B�LB��B��B�uB��B�oB�VB�DB�+Bs�Bl�BbNBYBYBQ�BG�B8RB5?B�B�B\B+BB��B��B��B�B�fB�TB�
BȴBǮBĜB��B�RB�3B�!B��B��B� Bl�B]/BT�BE�B5?BhB
�B
ƨB
�9B
��B
�bB
� B
p�B
n�B
gmB
dZB
YB
>wB
#�B
	7B
B	��B	�)B	�'B	��B	�%B	�B	{�B	v�B	p�B	hsB	6FB	$�B	�B	oB	  B	B�yB��B�
BɺB�dB�LB�?B�B�B��B��B��B�{B�hB�{B�DB�7B�B� B�Bz�B~�By�Bs�BbNBdZBcTBffBm�Bk�Bm�Bm�Bp�Bv�Bv�BgmBiyBk�Bn�BiyBffBk�Be`BgmBdZBaHBcTBbNBbNBaHBaHB`BB_;B]/B]/B^5B]/B]/B]/B]/B\)BZBZB[#B]/B\)B\)B_;BdZBZB\)B^5B_;B^5B^5BcTBe`B]/BdZB\)B`BB]/B_;BZB[#BXBXBVBXBT�BZBXBVBN�BE�BI�BK�BJ�BG�BB�B?}B=qB?}BA�BB�B9XB9XB7LB;dB7LB6FB;dBA�BO�BD�B:^B;dB;dB>wB?}BiyBp�Bs�Bt�Bw�B�B�7B�%B�+B�7B�bB�1B�DB�B�B�B�+B�1B�%B�JB�oB��B�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�'B�?B�XB�^B�qB��BÖBƨBȴB��B�B�)B�NB�`B�B�B��B��B��B��B	B	B	DB	PB	PB	hB	�B	�B	�B	�B	�B	�B	!�B	%�B	'�B	+B	#�B	#�B	&�B	&�B	)�B	+B	�B	uB	uB	bB	bB	oB	oB	oB	{B	�B	�B	�B	%�B	'�B	+B	-B	:^B	C�B	D�B	F�B	H�B	I�B	H�B	L�B	N�B	O�B	P�B	S�B	W
B	XB	XB	\)B	_;B	dZB	ffB	k�B	l�B	o�B	p�B	u�B	z�B	|�B	}�B	�B	�B	�%B	�1B	�DB	�PB	�hB	�uB	�oB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�?B	�LB	�XB	�XB	�XB	�^B	�qB	�wB	��B	��B	��B	��B	��B	��B	B	ÖB	ÖB	ĜB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�/B	�5B	�BB	�HB	�BB	�TB	�TB	�NB	�TB	�ZB	�ZB	�`B	�mB	��B
B
PB
�B
�B
)�B
/B
6FB
<jB
C�B
H�B
N�B
R�B
YB
^5B
bNB
ffB
l�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452172012011014521720120110145217  AO  ARGQ                                                                        20111130144211  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144211  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145217  IP                  G�O�G�O�G�O�                