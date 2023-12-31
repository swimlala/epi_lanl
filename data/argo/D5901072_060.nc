CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:55Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               <A   AO  20111130144119  20190522121829  1728_5048_060                   2C  D   APEX                            2142                            040306                          846 @Կ��b@1   @Կ�@@4������b�;dZ�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7y�D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DC��DDy�DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwl�Dy��D�3D�0 D��3D���D���D�#3D�` D��fD��fD�I�D�p Dǜ�D���D�C3D�ffD�fD��fD��D�\�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�33@�33A��A9��AY��Ax  A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�ffB�33B�33B�33B�33B�33B�33B�33B�33B�  B�33B�ffB�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C�ٚC���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���D ffD �fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD��DffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7` D7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB�fDCffDC� DD` DD� DEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM�fDNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwS3Dys3D��fD�#3D��fD�� D�� D�fD�S3D���D��D�<�D�c3Dǐ D�� D�6fD�Y�D���D�ٚD� D�P 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�n�A�t�A�v�A�x�A�v�A��A��7A��DA��PA��PA��PA��uA���A���A���A��A��A�-A���A���A�(�A���A��+A�\)A�VA��A���A��hA�z�A��A�p�A�z�A�dZA�ZA�ZA�M�A�"�A�JA�
=A�{A�1A��
A���A�ȴA��FA���A��DA��A�r�A�ZA�?}A��A���A�|�A�l�A�ZA�K�A�G�A�A�A�/A�A��TA�G�A��TA�E�A�ƨA�  A�1A���A��DA���A�9XA��A���A��FA��\A��/A�A�A���A��uA�/A��A��mA�S�A��;A�z�A�ZA�x�A�1A�ƨA��7A�M�A��;A�v�A�  A�/A��A�VA��wA�l�A���A�~�A��A���A�-A�x�A�{A��7A�=qA�$�A�ȴA��DA��A�ZA���A��HA��FA�"�At�A|  Aw\)Av1As;dAo��Am�wAl9XAjbNAh�9Ag33Aa��A_33A[\)AX�DAX5?AW�AW��AW33AS�wARI�AQl�AN�`ALz�AJ=qAH�AF��AF9XAEG�AB�A?t�A<��A9x�A7�A7\)A6��A6I�A4�uA3\)A2�!A2$�A0�A0jA/p�A.v�A,��A,=qA+��A+hsA*��A*I�A)�A(�A'�A'oA%�mA$^5A"v�A!C�A 9XA\)A��A{AK�A5?A\)AƨAĜA|�A�HA��A1'AbNA�
A��A�A��A�7A�A�A�-AVA
�\A
-A	�A	C�A�A��AdZA�jAG�A9XA��A�AbAS�A �9A 1@��;@���@��@�1'@�;d@�&�@�F@�=q@�u@�\@�p�@�9X@���@�(�@���@��@�  @��@��@�t�@���@�`B@� �@�33@��#@ؼj@�ƨ@�~�@�j@�n�@�j@Ͳ-@���@���@���@��@Ĵ9@°!@�`B@��@�r�@�b@�\)@�+@��R@��@�&�@��D@�ƨ@���@�33@��y@���@�ȴ@��\@���@��@�j@� �@�ƨ@�\)@�"�@�5?@���@��h@��7@��@�`B@�?}@��9@�I�@�ƨ@�S�@���@���@���@��@��@�V@��@���@�t�@�(�@�ƨ@�+@���@���@�9X@�o@�ff@�@���@��9@�O�@�bN@�9X@�b@�j@��`@��@� �@��m@���@���@���@�=q@��@�%@�1@��P@�+@��@�v�@�-@�@���@�`B@�&�@��@��@��9@��@�r�@�I�@�  @���@�dZ@�33@���@��\@�J@�hs@��@���@�1'@���@�;d@��R@�v�@��\@�v�@�@���@�X@�?}@���@�^5@���@��@���@�33@��F@��@�33@�;d@�t�@�dZ@�33@�@�n�@���@�hs@��7@���@�x�@��@�V@��@���@��j@���@�1'@�dZ@�C�@�33@�33@��@�
=@�@�o@�
=@��@��@��!@��+@�ff@�^5@�E�@�@��@��@��-@�X@�V@��`@���@��`@��@��`@��@��`@���@�j@�(�@��@�ƨ@��F@�ƨ@���@���@��w@���@�dZ@�;d@���@���@�n�@�=q@�$�@�{@�@��T@���@�hs@�%@��j@��@��
@�ȴ@�V@�E�@��!@���@�~�@�V@�{@��@�E�@�E�@��@�O�@��j@�A�@�1@��@���@�S�@�+@�+@�"�@���@��!@�~�@�^5@�5?@��@��@�@���@���@��7@�hs@�/@��@��j@��@���@�Q�@��;@�ƨ@��@��P@�\)@��@��R@�M�@�5?@��#@x �@p��@hb@b^5@\��@St�@K�F@C�m@<��@5�h@0  @+��@%�-@"��@��@x�@1@�P@(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�p�A�n�A�t�A�v�A�x�A�v�A��A��7A��DA��PA��PA��PA��uA���A���A���A��A��A�-A���A���A�(�A���A��+A�\)A�VA��A���A��hA�z�A��A�p�A�z�A�dZA�ZA�ZA�M�A�"�A�JA�
=A�{A�1A��
A���A�ȴA��FA���A��DA��A�r�A�ZA�?}A��A���A�|�A�l�A�ZA�K�A�G�A�A�A�/A�A��TA�G�A��TA�E�A�ƨA�  A�1A���A��DA���A�9XA��A���A��FA��\A��/A�A�A���A��uA�/A��A��mA�S�A��;A�z�A�ZA�x�A�1A�ƨA��7A�M�A��;A�v�A�  A�/A��A�VA��wA�l�A���A�~�A��A���A�-A�x�A�{A��7A�=qA�$�A�ȴA��DA��A�ZA���A��HA��FA�"�At�A|  Aw\)Av1As;dAo��Am�wAl9XAjbNAh�9Ag33Aa��A_33A[\)AX�DAX5?AW�AW��AW33AS�wARI�AQl�AN�`ALz�AJ=qAH�AF��AF9XAEG�AB�A?t�A<��A9x�A7�A7\)A6��A6I�A4�uA3\)A2�!A2$�A0�A0jA/p�A.v�A,��A,=qA+��A+hsA*��A*I�A)�A(�A'�A'oA%�mA$^5A"v�A!C�A 9XA\)A��A{AK�A5?A\)AƨAĜA|�A�HA��A1'AbNA�
A��A�A��A�7A�A�A�-AVA
�\A
-A	�A	C�A�A��AdZA�jAG�A9XA��A�AbAS�A �9A 1@��;@���@��@�1'@�;d@�&�@�F@�=q@�u@�\@�p�@�9X@���@�(�@���@��@�  @��@��@�t�@���@�`B@� �@�33@��#@ؼj@�ƨ@�~�@�j@�n�@�j@Ͳ-@���@���@���@��@Ĵ9@°!@�`B@��@�r�@�b@�\)@�+@��R@��@�&�@��D@�ƨ@���@�33@��y@���@�ȴ@��\@���@��@�j@� �@�ƨ@�\)@�"�@�5?@���@��h@��7@��@�`B@�?}@��9@�I�@�ƨ@�S�@���@���@���@��@��@�V@��@���@�t�@�(�@�ƨ@�+@���@���@�9X@�o@�ff@�@���@��9@�O�@�bN@�9X@�b@�j@��`@��@� �@��m@���@���@���@�=q@��@�%@�1@��P@�+@��@�v�@�-@�@���@�`B@�&�@��@��@��9@��@�r�@�I�@�  @���@�dZ@�33@���@��\@�J@�hs@��@���@�1'@���@�;d@��R@�v�@��\@�v�@�@���@�X@�?}@���@�^5@���@��@���@�33@��F@��@�33@�;d@�t�@�dZ@�33@�@�n�@���@�hs@��7@���@�x�@��@�V@��@���@��j@���@�1'@�dZ@�C�@�33@�33@��@�
=@�@�o@�
=@��@��@��!@��+@�ff@�^5@�E�@�@��@��@��-@�X@�V@��`@���@��`@��@��`@��@��`@���@�j@�(�@��@�ƨ@��F@�ƨ@���@���@��w@���@�dZ@�;d@���@���@�n�@�=q@�$�@�{@�@��T@���@�hs@�%@��j@��@��
@�ȴ@�V@�E�@��!@���@�~�@�V@�{@��@�E�@�E�@��@�O�@��j@�A�@�1@��@���@�S�@�+@�+@�"�@���@��!@�~�@�^5@�5?@��@��@�@���@���@��7@�hs@�/@��@��j@��@���@�Q�@��;@�ƨ@��@��P@�\)@��@��R@�M�@�5?@��#@x �@p��@hb@b^5@\��@St�@K�F@C�m@<��@5�h@0  @+��@%�-@"��@��@x�@1@�P@(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB>wB>wB>wB?}B?}B@�BB�BC�BC�BC�BC�BF�BH�BK�BQ�BbNB�LBBhB�B$�B1'B9XB>wB7LB6FB5?B5?B8RB@�BC�BJ�BH�BJ�BN�BP�BN�BN�BS�B\)B_;BdZBgmBgmBjBk�Bm�Bo�Bu�B}�B��B��B��B��B��B�B�3B�FB�XB�wBƨBŢB��B�wBŢB�9B�=Bl�B`BBjBl�BbNBO�B?}B8RB2-B9XBE�BE�BH�BF�BK�BQ�B[#BVBR�BC�BE�BC�B@�B?}B7LB;dB49B,B�BDB+BB��B�B�;B��B��B� BgmBK�B@�B&�B
��B
��B
�3B
�JB
|�B
n�B
T�B
L�B
+B

=B	�B	B	�XB	��B	�DB	x�B	hsB	bNB	aHB	VB	C�B	&�B	bB	
=B	+B	+B	�B	6FB	 �B	uB	{B	JB��B�yB�NB�B�)B�NB��BƨB�XB�-B��B��B��B��B��B�{B��B�oB��B�JB�bB�hB�PB�=B�JB�VB�oB��B�uB��B�=B�=B� Bx�Bk�BhsBn�Bk�BiyBhsBe`BffBdZB[#BT�BS�BQ�BP�BN�BL�BL�BL�BI�BF�BE�BC�BB�BB�BB�BC�BD�BC�BD�BA�BB�BC�BF�BG�B@�B=qB@�BB�B<jB;dBA�B=qB9XB8RB;dB:^B<jB8RB:^B8RB5?B7LB7LB33B1'B2-B7LB6FB5?B7LB8RB7LB7LB8RB7LB6FB7LB7LB8RB:^B6FB5?B5?B6FB@�BD�BD�B>wBF�BE�BB�BG�BK�BR�BL�BM�BS�BYB_;BZBYB]/B`BB`BB_;B`BBn�Bo�Bp�Bp�Bs�Bw�Bv�B{�Bz�B|�B~�B�B�7B�PB�{B�bB�bB�hB�uB�hB�uB�uB�uB��B��B��B�-B�RB�^B�^B�XB�XB�qB�^B�LB�LB�FB�LB��B��BÖBǮB��B��B��B��B��B��B�B��B��B��B��B�B�/B�BB�NB�TB�`B�sB�yB�B�B�B��B��B��B��B��B	B	B	%B	JB	\B	hB	�B	�B	�B	!�B	 �B	$�B	+B	0!B	2-B	5?B	9XB	:^B	:^B	?}B	G�B	M�B	R�B	S�B	ZB	bNB	iyB	n�B	p�B	q�B	s�B	v�B	w�B	z�B	{�B	|�B	z�B	|�B	�B	�1B	�7B	�JB	�bB	�bB	�hB	�oB	�oB	�uB	�hB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�9B	�?B	�LB	�LB	�LB	�RB	�dB	�jB	�jB	�qB	�wB	��B	ŢB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�B	�B	�#B	�)B	�B	�B	�#B	�5B	�;B	�5B	�5B	�5B	�;B	�TB	�ZB	�ZB	�ZB	�NB	�HB	�BB	�BB	�;B	�;B	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
B
DB
�B
�B
$�B
-B
5?B
<jB
D�B
M�B
R�B
W
B
]/B
`BB
e`B
iyB
n�B
t�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B>wB>wB>wB>wB?}B?}B@�BB�BC�BC�BC�BC�BF�BH�BK�BQ�BaHB�LB+BoB�B&�B2-B:^B@�B8RB7LB6FB6FB8RBA�BC�BK�BH�BJ�BN�BQ�BO�BN�BS�B\)B`BBdZBgmBhsBk�Bl�Bm�Bo�Bv�B~�B��B��B��B��B��B�B�3B�FB�^B�}BǮBȴB��B��B��B��B�uBv�BffBn�Bp�BhsBXBD�B9XB49B;dBF�BG�BJ�BI�BN�BT�B]/BXBYBH�BG�BE�BB�B@�B:^B=qB7LB1'B#�B\B
=BB��B�B�yB�B��B�7Bo�BN�BH�B7LB+B
��B
�}B
�uB
�B
t�B
W
B
W
B
49B
�B	��B	ǮB	ÖB	�-B	�oB	~�B	o�B	iyB	hsB	iyB	N�B	5?B	�B	JB	1B	1B	�B	@�B	$�B	�B	�B	oB	B�B�mB�)B�;B�yB�/B��B��B�?B��B��B��B��B��B��B��B��B��B�\B�uB��B�\B�JB�VB�bB�{B��B��B��B�PB�VB�B~�Bo�Bk�Bq�Bm�Bk�Bk�BiyBiyBiyB^5BYBVBR�BR�BT�BN�BM�BP�BO�BJ�BI�BI�BG�BE�BD�BE�BF�BF�BH�BC�BD�BF�BL�BK�BB�B@�BD�BE�B?}B?}BI�BA�B;dB:^B>wB>wB?}B;dB=qB;dB7LB9XB;dB6FB33B49B:^B8RB8RB:^B9XB9XB9XB:^B9XB8RB9XB9XB;dB=qB9XB9XB8RB:^BC�BF�BF�B@�BH�BF�BC�BH�BL�BS�BM�BN�BT�BZB`BB[#BZB^5BaHB`BB`BBbNBo�Bp�Bq�Bq�Bt�Bx�Bx�B|�B{�B|�B~�B�B�=B�VB��B�hB�hB�oB��B�uB��B��B�{B��B��B��B�'B�XB�dB�dB�dB�jB�}B�dB�RB�XB�LB�FBÖBBĜBǮB��B��B��B��B��B��B�#B��B��B��B�B�B�5B�HB�TB�ZB�fB�yB�B�B�B�B��B��B��B��B	  B	B	B	+B	PB	bB	oB	�B	�B	�B	"�B	!�B	%�B	,B	1'B	2-B	5?B	:^B	;dB	;dB	?}B	F�B	M�B	S�B	T�B	ZB	aHB	iyB	n�B	q�B	q�B	s�B	v�B	x�B	{�B	|�B	~�B	{�B	|�B	�B	�7B	�=B	�JB	�bB	�bB	�oB	�uB	�uB	��B	�oB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�3B	�?B	�FB	�RB	�LB	�LB	�RB	�dB	�jB	�jB	�qB	�wB	��B	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�)B	�5B	�B	�B	�#B	�5B	�;B	�;B	�;B	�5B	�;B	�TB	�`B	�`B	�`B	�TB	�NB	�BB	�HB	�BB	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�NB	�TB	�ZB	�TB	�TB	�ZB	�`B	�`B	�`B	�fB	�fB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B
B
DB
�B
�B
$�B
.B
5?B
<jB
D�B
M�B
R�B
XB
]/B
`BB
e`B
iyB
o�B
t�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<e`B<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<49X<u<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<���<#�
<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452142012011014521420120110145214  AO  ARGQ                                                                        20111130144119  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144119  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145214  IP                  G�O�G�O�G�O�                