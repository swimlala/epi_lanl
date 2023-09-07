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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               >A   AO  20111130144132  20190522121829  1728_5048_062                   2C  D   APEX                            2142                            040306                          846 @���O@ 1   @����?�@5�t�j�b��1'1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DB��DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DNfDN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy��D� D�C3D�s3D��fD���D�  D�\�D���D�3D�<�D�` Dǳ3D��3D�fDچfD�3D�ɚD�fD�Y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  @�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBFffBNffBVffB^ffBfffBnffBvffB~ffB�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33B�33C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]�3C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C�� C���C���C���C���C���C���C���C���C���C���C���C���C�� C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ٚC���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDl�D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD	ffD	�fD
ffD
�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD` D�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fDffD�fD ffD �fD!ffD!�fD"ffD"�fD#ffD#�fD$ffD$�fD%ffD%�fD&ffD&�fD'ffD'�fD(ffD(�fD)ffD)�fD*ffD*�fD+ffD+�fD,ffD,�fD-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1ffD1�fD2ffD2�fD3ffD3�fD4ffD4�fD5ffD5�fD6ffD6�fD7ffD7�fD8ffD8�fD9ffD9�fD:ffD:�fD;ffD;�fD<ffD<�fD=ffD=�fD>ffD>�fD?ffD?�fD@ffD@�fDAffDA�fDBffDB� DCffDC�fDDffDD�fDEffDE�fDFffDF�fDGffDG�fDHffDH�fDIffDI�fDJffDJ�fDKffDK�fDLffDL�fDMffDM��DNffDN�fDOffDO�fDPffDP�fDQffDQ�fDRffDR�fDSffDS�fDTffDT�fDUffDU�fDVffDV�fDWffDW�fDXffDX�fDYffDY�fDZffDZ�fD[ffD[�fD\ffD\�fD]ffD]�fD^ffD^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi�fDjffDj�fDkffDk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDqffDq�fDrffDr�fDsffDs�fDtffDt�fDuffDu�fDvffDv�fDwffDy�3D�3D�6fD�ffD���D�� D�3D�P D���D��fD�0 D�S3DǦfD��fD�	�D�y�D��fD��D���D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��DA���A���A���A���A���A���A�=qA���A� �A��;A�p�A�VA�O�A�K�A�E�A�A�A�C�A�E�A�C�A�?}A�C�A�O�A�Q�A�I�A�9XA�/A�-A�"�A��A��A�bA�
=A�A���A��hA���A�n�A�bA�A�A�(�A�ĜA�9XA��PA���A�33A�
=A��FA���A�1'A�oA�5?A�I�A��A�dZA��wA��7A��A�+A��A���A���A�C�A��9A�VA���A��^A�S�A���A�hsA�?}A���A��A�ƨA�p�A��yA�hsA�?}A��A�+A�hsA��^A��hA�C�A��mA���A�p�A��^A��DA���A�t�A�
=A���A�t�A�/A��A���A�&�A��PA���A��A�=qA��+A��DA�ĜA�E�A�XA��uA���A��hA�  A��A��/A��A�A�XA�33A~JA|jA{Ax-Aq�Ap{Am�wAj�jAhv�AhQ�Ag��Af��AfJAc��A` �A^��A^ZA^  A]VAZ1'AW��AVv�AU|�AVAV5?AU�AT��ASt�AQx�AP�AO��ANVAM��AK�TAJ�AJ��AJVAI�FAH�`AG�AG�AGoAF��AES�AD��ADJAA`BA>��A<�9A;x�A:  A8ffA5�A4  A25?A0�\A/33A.�A,�`A+p�A)A'��A&�\A%XA$��A$�A#A"$�A!��A!�A+Ar�AZA=qAdZA�A�hA�^A�A  A�RAQ�A�A�A(�Al�AjAx�AQ�A��A&�A�`A��AdZA(�A��AC�AoA
�A	%AVA;dA�+A�wA�AM�A��A �A ~�A 1'@�
=@���@�C�@�&�@�-@�D@�n�@�7@�@�X@�j@�j@��m@���@���@�p�@���@�;d@��@��
@◍@�@���@�A�@ߕ�@��y@�/@��@��/@�S�@��@���@�x�@��@�-@́@�I�@�ƨ@�33@ɑh@�b@�v�@ũ�@Ĭ@�9X@���@î@�\)@�v�@�@��@��
@�;d@��@�V@��@�b@�S�@���@�J@���@���@�|�@�ȴ@�~�@�=q@��T@�`B@�&�@���@�r�@�1'@���@��!@�=q@���@���@�r�@���@�t�@�33@�~�@��7@��j@�Q�@�1'@���@�Z@��@��H@��\@�V@�?}@�Z@�(�@��;@�v�@�`B@�(�@�~�@���@��@�1'@�"�@�M�@��@���@��@�Q�@� �@�b@��@�|�@�o@��R@�^5@�@��@���@���@�X@��@�r�@���@�dZ@�"�@��y@���@���@��!@�v�@�5?@�{@��@��@���@�-@�V@���@��y@��@�33@�+@�+@��@��@���@���@��+@�~�@�v�@�v�@�v�@�ff@�E�@��@���@�p�@�V@�V@���@���@���@�Z@�(�@�  @���@���@�l�@�33@�@��@��@�ȴ@��!@��\@�E�@��T@���@���@��h@�O�@�/@�%@��j@�r�@��@���@�S�@�
=@�ȴ@�~�@�{@���@���@��7@�/@��/@��u@�bN@�(�@��m@��w@��P@�K�@��@��@��R@�v�@�5?@���@��h@�x�@�G�@�/@��@���@���@��@�A�@� �@��@���@��@���@�|�@�C�@�33@���@��R@�M�@�$�@���@���@�/@���@��/@��/@��`@���@��@�I�@�1@��;@��w@��w@���@���@�|�@�\)@�+@���@��!@�n�@�5?@�{@��@���@��^@���@�x�@�`B@�G�@��@���@��`@�b@u`B@m?}@d��@Z-@T��@M@G
=@AX@:��@3��@.$�@(1'@#ƨ@�@X@��@��@��@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��DA���A���A���A���A���A���A�=qA���A� �A��;A�p�A�VA�O�A�K�A�E�A�A�A�C�A�E�A�C�A�?}A�C�A�O�A�Q�A�I�A�9XA�/A�-A�"�A��A��A�bA�
=A�A���A��hA���A�n�A�bA�A�A�(�A�ĜA�9XA��PA���A�33A�
=A��FA���A�1'A�oA�5?A�I�A��A�dZA��wA��7A��A�+A��A���A���A�C�A��9A�VA���A��^A�S�A���A�hsA�?}A���A��A�ƨA�p�A��yA�hsA�?}A��A�+A�hsA��^A��hA�C�A��mA���A�p�A��^A��DA���A�t�A�
=A���A�t�A�/A��A���A�&�A��PA���A��A�=qA��+A��DA�ĜA�E�A�XA��uA���A��hA�  A��A��/A��A�A�XA�33A~JA|jA{Ax-Aq�Ap{Am�wAj�jAhv�AhQ�Ag��Af��AfJAc��A` �A^��A^ZA^  A]VAZ1'AW��AVv�AU|�AVAV5?AU�AT��ASt�AQx�AP�AO��ANVAM��AK�TAJ�AJ��AJVAI�FAH�`AG�AG�AGoAF��AES�AD��ADJAA`BA>��A<�9A;x�A:  A8ffA5�A4  A25?A0�\A/33A.�A,�`A+p�A)A'��A&�\A%XA$��A$�A#A"$�A!��A!�A+Ar�AZA=qAdZA�A�hA�^A�A  A�RAQ�A�A�A(�Al�AjAx�AQ�A��A&�A�`A��AdZA(�A��AC�AoA
�A	%AVA;dA�+A�wA�AM�A��A �A ~�A 1'@�
=@���@�C�@�&�@�-@�D@�n�@�7@�@�X@�j@�j@��m@���@���@�p�@���@�;d@��@��
@◍@�@���@�A�@ߕ�@��y@�/@��@��/@�S�@��@���@�x�@��@�-@́@�I�@�ƨ@�33@ɑh@�b@�v�@ũ�@Ĭ@�9X@���@î@�\)@�v�@�@��@��
@�;d@��@�V@��@�b@�S�@���@�J@���@���@�|�@�ȴ@�~�@�=q@��T@�`B@�&�@���@�r�@�1'@���@��!@�=q@���@���@�r�@���@�t�@�33@�~�@��7@��j@�Q�@�1'@���@�Z@��@��H@��\@�V@�?}@�Z@�(�@��;@�v�@�`B@�(�@�~�@���@��@�1'@�"�@�M�@��@���@��@�Q�@� �@�b@��@�|�@�o@��R@�^5@�@��@���@���@�X@��@�r�@���@�dZ@�"�@��y@���@���@��!@�v�@�5?@�{@��@��@���@�-@�V@���@��y@��@�33@�+@�+@��@��@���@���@��+@�~�@�v�@�v�@�v�@�ff@�E�@��@���@�p�@�V@�V@���@���@���@�Z@�(�@�  @���@���@�l�@�33@�@��@��@�ȴ@��!@��\@�E�@��T@���@���@��h@�O�@�/@�%@��j@�r�@��@���@�S�@�
=@�ȴ@�~�@�{@���@���@��7@�/@��/@��u@�bN@�(�@��m@��w@��P@�K�@��@��@��R@�v�@�5?@���@��h@�x�@�G�@�/@��@���@���@��@�A�@� �@��@���@��@���@�|�@�C�@�33@���@��R@�M�@�$�@���@���@�/@���@��/@��/@��`@���@��@�I�@�1@��;@��w@��w@���@���@�|�@�\)@�+@���@��!@�n�@�5?@�{@��@���@��^@���@�x�@�`B@�G�@��@���@��`@�b@u`B@m?}@d��@Z-@T��@M@G
=@AX@:��@3��@.$�@(1'@#ƨ@�@X@��@��@��@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�B
�B
� B
�B
�B
�B
�B
�B
��B
�BBVB�B#�B$�B$�B%�B(�B,B.B/B1'B5?B:^B<jB;dB;dB<jB=qB=qB=qB=qB=qB>wB?}BA�BC�BD�BI�BN�B�{B�jBɺB�B��B�qB�qB��B�
B�NB�sB�B+B�BJB��B�B�B��BPBuB�B�BoB��B�B�qB�{B�B�PB��B�B�jB�}BĜBȴB��B��B��B��BɺB��BƨB��BǮBȴB��B��B��B�B��B��BÖBȴBÖBB�}B�XB�3B��B��B�1B}�Bq�B\)BH�B=qB(�BB�BhB
�B
ƨB
�LB
�3B
�7B
dZB
I�B
(�B
bB
B	��B	�B	�LB	��B	��B	p�B	W
B	VB	W
B	I�B	>wB	$�B	\B	B	oB	�B	"�B	VB��B�B�B	B	�B	�B	�B	�B	bB	PB		7B	B��B	B��B	  B	1B	VB	VB	JB		7B		7B	1B	B	1B	B�TBȴB�qB�3B��B��B�\B�B� B�B�B�7B�=B�1B�B�1B�hB�1B�B�VB�B�+B�B�B}�B}�B}�B|�Bz�By�Bz�Bv�B}�Bv�Bm�Bk�Bk�Bn�Bm�Bl�Bl�Bk�Bl�Bp�Bn�BjBhsBm�Be`BdZBdZB`BB^5Be`Be`BbNBVBcTB\)BVBffBJ�BB�BA�B=qBA�BG�B8RB6FB7LB8RB6FB7LB7LB49B49B49B8RB5?B5?B49B49B33B33B2-B0!B2-B2-B33B2-B1'B2-B2-B0!B2-B5?B6FB9XB<jB<jBA�BA�BA�BB�BB�B@�BC�BB�BC�BG�BM�BR�BYB^5BaHBhsBffBiyBk�BiyBl�Bt�By�B}�B~�B�1B�7B�7B�=B�DB�VB�uB�{B��B��B��B��B��B��B��B��B�B�'B�?B�RB�jBÖBȴB��B��B��B�B�B�BB�BB�5B�)B�;B�fB�sB�B�B�B�yB�B�B�B�B�B�B�B�B�B��B��B��B	  B	B	B	
=B	hB	uB	�B	�B	�B	�B	#�B	+B	-B	0!B	2-B	49B	5?B	6FB	:^B	=qB	?}B	C�B	E�B	K�B	Q�B	T�B	XB	[#B	\)B	^5B	aHB	m�B	s�B	v�B	y�B	|�B	}�B	~�B	~�B	~�B	� B	� B	�B	�B	�1B	�DB	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�LB	�RB	�^B	�dB	�jB	�}B	��B	ÖB	ÖB	ÖB	ĜB	ŢB	ƨB	ƨB	ȴB	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�)B	�/B	�/B	�5B	�5B	�5B	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
%B
{B
�B
&�B
,B
2-B
9XB
@�B
H�B
O�B
S�B
[#B
_;B
cTB
k�B
l�B
q�B
t�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
� B
�B
�B
�B
�B
�%B
��B
�BBbB�B#�B$�B$�B%�B(�B,B.B/B1'B5?B:^B<jB;dB;dB<jB=qB=qB=qB=qB=qB>wB?}BC�BF�BF�BK�BM�B��B�wB��B�B�B�}B�wB��B�B�ZB�yB�BDB �B\B��B�B�B��BVB�B �B�B�BDB�mB��B��B�%B�\B��B�B�qB��BƨB��B��B��B��B��B��BÖBǮB��BɺBɺB��B�B�B�B�
B��BĜB��BŢBĜB��B�jB�LB�B��B�JB�Bw�BaHBQ�BC�B.BG�B�B
�)B
��B
�jB
�LB
�hB
m�B
N�B
.B
{B
B
B	�yB	�dB	�!B	�B	v�B	XB	XB	ZB	L�B	D�B	/B	uB	B	uB	�B	+B	�B��B�B�B	B	�B	�B	 �B	"�B	uB	bB	VB	B	B	%B��B	B	DB	hB	oB	VB	DB	DB	JB	+B	JB	JB�B��BB�RB�B��B��B�1B�%B�%B�7B�PB�VB�JB�=B�DB�{B�7B�1B�\B�7B�7B�+B�1B� B~�B~�B� B{�B� B�By�B�B{�Bo�Bm�Bo�Bq�Bp�Bp�Bp�Bo�Bn�Br�Bo�Bk�Bk�Bp�BgmBe`Be`BcTBaHBgmBhsBdZBXBe`B^5B[#Bk�BL�BC�BC�B@�BE�BK�B=qB9XB;dB:^B:^B;dB8RB5?B5?B6FB:^B6FB6FB7LB7LB6FB5?B49B2-B49B33B5?B5?B5?B6FB5?B49B6FB5?B9XB<jB>wB>wBB�BB�BD�BE�BE�BB�BE�BC�BD�BH�BN�BT�B[#B_;BcTBiyBgmBjBm�Bk�Bm�Bu�Bz�B~�B�B�=B�=B�=B�DB�JB�\B�{B��B��B��B��B��B��B��B��B��B�B�-B�FB�XB�wBŢBɺB��B��B��B�B�#B�HB�HB�BB�5B�BB�mB�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B	  B	B	B	B	DB	hB	{B	�B	�B	�B	�B	$�B	,B	.B	1'B	2-B	49B	5?B	7LB	;dB	=qB	@�B	C�B	E�B	K�B	Q�B	T�B	XB	[#B	\)B	^5B	aHB	m�B	t�B	w�B	y�B	|�B	}�B	~�B	~�B	~�B	� B	�B	�B	�B	�7B	�JB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�!B	�'B	�3B	�9B	�LB	�XB	�^B	�dB	�qB	��B	B	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�B	�/B	�/B	�5B	�5B	�5B	�;B	�HB	�BB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�fB	�`B	�fB	�mB	�mB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
%B
{B
�B
&�B
,B
2-B
9XB
A�B
H�B
P�B
S�B
[#B
_;B
cTB
k�B
m�B
q�B
t�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<T��<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452152012011014521520120110145215  AO  ARGQ                                                                        20111130144132  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144132  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145215  IP                  G�O�G�O�G�O�                