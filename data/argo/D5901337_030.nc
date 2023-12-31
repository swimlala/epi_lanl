CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:31Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       j    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       r   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |$   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111205112828  20190522121836  1901_5055_030                   2C  D   APEX                            2140                            040306                          846 @�sڢ�@1   @�s�>���@."M����c6��`A�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BG��BP  BXffB`  Bh  Bp  Bx  B�  B�  B�33B�  B�  B�  B�33B�  B���B�  B�  B���B�  B�33B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�33B�33C   C�fC  C  C�fC
  C  C�fC  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C;�fC>  C@�CB  CC�fCF  CH  CJ  CL  CN  CP�CR�CT  CV  CW�fCZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Cs�fCv  Cx�Cz�C|�C~�C��C��C�  C�  C��3C�  C�  C�  C��C��3C�  C�  C��C��C��3C�  C��C��C�  C��3C�  C��C�  C��3C��3C��3C�  C��C��C��C��C�  C�  C��3C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C��C�  C�  C��C�  C��3C�  C�  C��3C��C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C��C��C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C��C��3C��3C��3C��3C�  C�  C�  C�  C�  C��3C��3C�  C��C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  D� D  D�fD  D� D  D�fDfD� D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D��D� D  Dy�D  D� DfD� D  D� DfD� D��D� DfD� D  D� D   D � D!  D!� D"  D"� D#fD#� D#��D$� D%fD%�fD&  D&� D'  D'y�D'��D(y�D(��D)� D*  D*� D+  D+� D,  D,� D,��D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2fD2� D3  D3y�D4  D4� D4��D5� D6fD6� D7  D7� D8  D8y�D8��D9� D:  D:� D;  D;� D<  D<� D<��D=� D>  D>�fD?fD?�fD@fD@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DI  DI� DJ  DJy�DK  DK� DK��DL� DMfDM� DM��DN� DO  DO� DP  DP� DQ  DQy�DR  DR�fDS  DS� DT  DT� DT��DUy�DU��DVy�DV��DWy�DX  DX� DX��DY� DZ  DZ� D[  D[� D\fD\� D]  D]y�D^  D^�fD_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dj��Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dq��Dr� Ds  Dsy�Ds��Dt� Du  Du� Dv  Dv� Dw  Dw� Dy�fD��D�33D�p D�ɚD�  D�33D�ffD��3D��3D�,�D�i�D�� D��fD�  D�\�D�� D���D�fD�I�D�f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@fff@�33@�33A��A9��AY��Ay��A���A���A���A���A���A���A���A���BffBffBffBffB&ffB.ffB6ffB>ffBF  BNffBV��B^ffBfffBnffBvffB~ffB�33B�ffB�33B�33B�33B�ffB�33B�  B�33B�33B�  B�33B�ffB�33B�33B�33B�33B�ffB�33B�33B�33B�33B�  B�33B�33B�33B�33B�33B�33B�ffB�ffB�33C� C��C��C� C	��C��C� C��C��C��C�3C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3�3C5��C7��C9��C;� C=��C?�3CA��CC� CE��CG��CI��CK��CM��CO�3CQ�3CS��CU��CW� CY��C[��C]� C_��Ca��Cc��Ce��Cg��Ci��Ck�3Cm�3Co��Cq��Cs� Cu��Cw�3Cy�3C{�3C}�3C�3C�ٚC���C���C�� C���C���C���C�ٚC�� C���C���C�ٚC�ٚC�� C���C�ٚC�ٚC���C�� C���C�ٚC���C�� C�� C�� C���C�ٚC�ٚC�ٚC�ٚC���C���C�� C���C�ٚC���C���C���C���C�� C���C���C���C���C�� C���C�ٚC�ٚC�ٚC���C���C���C���C���C�� C�� C���C�ٚC���C�� C���C�ٚC���C���C�ٚC���C�� C���C���C�� C�ٚC���C���C���C�� C�� C�� C���C���C���C���C�� C���C���C���C���C�� C�ٚC�ٚC���C���C���C���C���C���C�ٚC�ٚC���C���C�ٚC���C���C�ٚC�� C�� C�� C�� C���C���C���C���C���C�� C�� C���C�ٚC�� C���C���C���C���C���C���C���C���C���C���C���D ffD �fDffD�fDffD��DffD�fDffD�fDl�D�fDffD�fDl�D��DffD� D	` D	� D
` D
� D` D� D` D� D` D� D` D� DffD�fDffD�fDffD�fDffD�fDffD�fDffD��DffD�fDffD� DffD�fD` D�fDffD��DffD�fDffD��DffD� DffD��DffD�fDffD�fD ffD �fD!ffD!�fD"ffD"��D#ffD#� D$ffD$��D%l�D%�fD&ffD&�fD'` D'� D(` D(� D)ffD)�fD*ffD*�fD+ffD+�fD,ffD,� D-ffD-�fD.ffD.�fD/ffD/�fD0ffD0�fD1l�D1��D2ffD2�fD3` D3�fD4ffD4� D5ffD5��D6ffD6�fD7ffD7�fD8` D8� D9ffD9�fD:ffD:�fD;ffD;�fD<ffD<� D=ffD=�fD>l�D>��D?l�D?��D@ffD@�fDAffDA�fDBffDB�fDCffDC�fDDffDD�fDEffDE�fDFffDF�fDG` DG�fDHffDH�fDIffDI�fDJ` DJ�fDKffDK� DLffDL��DMffDM� DNffDN�fDOffDO�fDPffDP�fDQ` DQ�fDRl�DR�fDSffDS�fDTffDT� DU` DU� DV` DV� DW` DW�fDXffDX� DYffDY�fDZffDZ�fD[ffD[��D\ffD\�fD]` D]�fD^l�D^�fD_ffD_�fD`ffD`�fDaffDa�fDbffDb�fDcffDc�fDdffDd�fDeffDe�fDfffDf�fDgffDg�fDhffDh�fDiffDi��DjffDj� Dk` Dk�fDlffDl�fDmffDm�fDnffDn�fDoffDo�fDpffDp�fDq` Dq� DrffDr�fDs` Ds� DtffDt�fDuffDu�fDvffDv�fDwffDy��D�  D�&fD�c3D���D��3D�&fD�Y�D��fD��fD�  D�\�Dǳ3D��D�3D�P D��3D�� D�	�D�<�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�ȴA���A�ƨA�ĜA�ĜA�ĜA�ȴA���A���A�ȴA���A���A���A���A���A�ȴA���A���A���A��A��/A��#A��#A��;A��/A���A�ƨA�^5A�(�A���A��AǙ�A�bNA�bNA�^5A�G�A�G�A��A���A�ƨAƅA���A�jA�Q�A�x�A§�A�ĜA�A�9XA�7LA��DA���A�r�A�x�A���A�VA�ĜA���A��A��#A��;A��A���A�p�A���A�dZA�5?A���A��`A��uA���A���A�ƨA��wA��A��TA�A�I�A���A�O�A� �A���A�oA���A�9XA���A�^5A��A��TA��A�;dA��wA�$�A���A��#A�O�A��uA��A�bNA��A{dZAy��AwK�Ar�AooAm%Ah��AfAd�A_x�A["�AWS�AS�-AP��AL �AGAF-AB�AAO�A>��A<��A;�;A:�+A8��A6��A6��A6I�A5A4��A3hsA2$�A1l�A0�RA/�A.  A,1A)&�A'&�A&(�A%A$�yA#�A\)AjA�A A��A��Al�A^5A��A`BAl�AƨAn�A�RA�RAv�AbAK�Av�A$�A��A��A7LAG�AoA�yA�yA�hAr�A�TA��A`BAA�9Av�A-At�A��A�
A+A��A�A�^A��A`BA��A�A1'A�A��AdZAC�A�A�A��A|�A;dA�DA{A��A�PAl�A/AoA
��A
VA
  A	��A	hsA�A�At�Al�AC�A�A7LA�A��AA�A�TA��A�yA(�A�PAXAVA��A�Av�A�A�FAXA ��A   @�l�@�
=@��H@�^5@��@�Ĝ@���@��P@��@��h@�z�@��
@�|�@��H@���@�7L@�/@�7L@�?}@���@�F@�O�@��@�bN@�dZ@�
=@��@�V@�@���@���@��@��@�h@��`@�z�@��@�@�j@���@�^5@�ff@�/@�1'@�t�@◍@ᙚ@�Z@���@߶F@ߕ�@���@�ff@�E�@���@�$�@�^5@�-@�-@ݩ�@��;@ڗ�@��@٩�@�@��@�b@ם�@�K�@���@��T@�A�@��;@��@��@���@�C�@Ο�@�%@�I�@�b@˅@�33@�ȴ@�5?@�$�@ɩ�@��@�Q�@��m@�|�@ư!@��@�G�@ļj@þw@�;d@��@�n�@���@��h@���@��D@�r�@�(�@�1@��
@��w@���@�
=@�v�@�5?@��T@�hs@�&�@���@��j@�I�@�ƨ@��@�
=@�-@��^@�7L@�I�@��F@�"�@�n�@�@��^@���@�%@��j@�9X@�ƨ@�K�@�33@�
=@��@��!@�{@�@�x�@�O�@���@���@��D@�z�@� �@���@���@�33@���@�~�@���@�M�@���@�7L@���@�Q�@�Q�@�r�@�(�@�ƨ@�;d@���@��\@�=q@���@�G�@�?}@��@��-@�V@�(�@�\)@��@�^5@���@�hs@�%@���@�V@�Ĝ@�r�@�G�@��j@��m@�\)@�C�@��@���@�5?@�G�@���@�9X@���@�t�@��@���@�ff@�{@�@��7@��@��`@��u@�j@�Q�@�A�@�  @�ƨ@��@�33@��y@���@��!@��+@�=q@���@���@���@���@���@���@��7@�V@��`@��`@���@���@�z�@�j@�9X@��;@���@�t�@���@�E�@��@���@���@�p�@�G�@�7L@�%@��j@���@�b@��w@�|�@�S�@�@���@�@���@���@��^@���@��`@�\)@|�@vv�@l(�@a��@["�@R�\@K33@BM�@<�@7\)@/�@*M�@$�/@�+@�#@�D@�u@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�ȴA���A�ƨA�ĜA�ĜA�ĜA�ȴA���A���A�ȴA���A���A���A���A���A�ȴA���A���A���A��A��/A��#A��#A��;A��/A���A�ƨA�^5A�(�A���A��AǙ�A�bNA�bNA�^5A�G�A�G�A��A���A�ƨAƅA���A�jA�Q�A�x�A§�A�ĜA�A�9XA�7LA��DA���A�r�A�x�A���A�VA�ĜA���A��A��#A��;A��A���A�p�A���A�dZA�5?A���A��`A��uA���A���A�ƨA��wA��A��TA�A�I�A���A�O�A� �A���A�oA���A�9XA���A�^5A��A��TA��A�;dA��wA�$�A���A��#A�O�A��uA��A�bNA��A{dZAy��AwK�Ar�AooAm%Ah��AfAd�A_x�A["�AWS�AS�-AP��AL �AGAF-AB�AAO�A>��A<��A;�;A:�+A8��A6��A6��A6I�A5A4��A3hsA2$�A1l�A0�RA/�A.  A,1A)&�A'&�A&(�A%A$�yA#�A\)AjA�A A��A��Al�A^5A��A`BAl�AƨAn�A�RA�RAv�AbAK�Av�A$�A��A��A7LAG�AoA�yA�yA�hAr�A�TA��A`BAA�9Av�A-At�A��A�
A+A��A�A�^A��A`BA��A�A1'A�A��AdZAC�A�A�A��A|�A;dA�DA{A��A�PAl�A/AoA
��A
VA
  A	��A	hsA�A�At�Al�AC�A�A7LA�A��AA�A�TA��A�yA(�A�PAXAVA��A�Av�A�A�FAXA ��A   @�l�@�
=@��H@�^5@��@�Ĝ@���@��P@��@��h@�z�@��
@�|�@��H@���@�7L@�/@�7L@�?}@���@�F@�O�@��@�bN@�dZ@�
=@��@�V@�@���@���@��@��@�h@��`@�z�@��@�@�j@���@�^5@�ff@�/@�1'@�t�@◍@ᙚ@�Z@���@߶F@ߕ�@���@�ff@�E�@���@�$�@�^5@�-@�-@ݩ�@��;@ڗ�@��@٩�@�@��@�b@ם�@�K�@���@��T@�A�@��;@��@��@���@�C�@Ο�@�%@�I�@�b@˅@�33@�ȴ@�5?@�$�@ɩ�@��@�Q�@��m@�|�@ư!@��@�G�@ļj@þw@�;d@��@�n�@���@��h@���@��D@�r�@�(�@�1@��
@��w@���@�
=@�v�@�5?@��T@�hs@�&�@���@��j@�I�@�ƨ@��@�
=@�-@��^@�7L@�I�@��F@�"�@�n�@�@��^@���@�%@��j@�9X@�ƨ@�K�@�33@�
=@��@��!@�{@�@�x�@�O�@���@���@��D@�z�@� �@���@���@�33@���@�~�@���@�M�@���@�7L@���@�Q�@�Q�@�r�@�(�@�ƨ@�;d@���@��\@�=q@���@�G�@�?}@��@��-@�V@�(�@�\)@��@�^5@���@�hs@�%@���@�V@�Ĝ@�r�@�G�@��j@��m@�\)@�C�@��@���@�5?@�G�@���@�9X@���@�t�@��@���@�ff@�{@�@��7@��@��`@��u@�j@�Q�@�A�@�  @�ƨ@��@�33@��y@���@��!@��+@�=q@���@���@���@���@���@���@��7@�V@��`@��`@���@���@�z�@�j@�9X@��;@���@�t�@���@�E�@��@���@���@�p�@�G�@�7L@�%@��j@���@�b@��w@�|�@�S�@�@���@�@���@���@��^@���@��`@�\)@|�@vv�@l(�@a��@["�@R�\@K33@BM�@<�@7\)@/�@*M�@$�/@�+@�#@�D@�u@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�B
,B
:^B
[#B
jB
m�B
n�B
p�B
q�B
y�B
~�B
�B
�+B
�PB
��B
�3B
�B
�B
�B�B$�B-B-B �B%�B.BT�BR�BN�BXBs�B��B��B��B��B��B�9BĜBĜB��B�dB��B�;B��BDB'�B2-B/BO�B2-BB�HB�
B��BĜB�XB�?B��B�1B�7B�1Bn�B`BBQ�BD�B9XB!�B
��B
��B
��B
�B
T�B
VB	�TB	��B	�jB	��B	�DB	z�B	ffB	T�B	C�B	,B	hB��B�mB��B�}B�)B��B	�B	�B	/B	7LB	9XB	7LB	:^B	D�B	H�B	L�B	O�B	Q�B	S�B	VB	VB	S�B	Q�B	O�B	F�B	8RB	-B	&�B	!�B	!�B	-B	%�B	'�B	A�B	bNB	iyB	k�B	x�B	� B	�=B	��B	��B	�B	B	��B	�B	�#B	�#B	�B	�)B	�5B	�TB	�sB	�B	�B	�B	�B	�B	�TB	�#B	�B	�B	�B	�#B	�`B	�B	�B	�sB	�sB	�B	�B	�B	�B	��B	��B	��B
B
	7B
DB
JB
\B
�B
{B
oB
oB
hB
oB
oB
VB
\B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
,B
.B
,B
)�B
'�B
&�B
!�B
�B
�B
#�B
#�B
"�B
!�B
 �B
!�B
"�B
 �B
�B
�B
�B
�B
�B
�B
oB
\B
PB
bB
\B
JB
1B
+B
+B
%B
+B
%B
1B
	7B
	7B
1B
+B
B
B
B	��B	��B	��B	��B	��B
B
	7B
JB
PB
PB
JB
DB
	7B
%B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�sB	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�`B	�TB	�5B	�B	�B	��B	��B	�B	�
B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�#B	�#B	�#B	�)B	�)B	�/B	�/B	�;B	�NB	�TB	�TB	�ZB	�ZB	�ZB	�ZB	�fB	�fB	�fB	�fB	�mB	�mB	�mB	�sB	�sB	�fB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
+B
+B
%B
B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B

=B

=B
DB
PB
PB
PB
PB
PB
PB
PB
PB
PB
PB
JB
JB
PB
PB
VB
VB
VB
VB
\B
bB
bB
oB
hB
hB
bB
bB
\B
bB
oB
oB
oB
{B
�B
 �B
"�B
+B
2-B
8RB
<jB
J�B
J�B
S�B
YB
`BB
cTB
gmB
k�B
n�B
q�B
v�B
z�B
}�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�NB	��B
0!B
@�B
_;B
l�B
m�B
n�B
q�B
r�B
{�B
�B
�B
�=B
�uB
��B
�qB
�NB
�B
�B�B+B9XB?}B/B/BA�Bz�Bo�BffBn�B�1B��B�3B�B��B�'B�dBɺBɺB��B��B�HB�mB��BVB33B;dB9XBffBG�BoB�fB�/B�TB��BÖBƨB�'B�bB��B��B}�Bs�BZBQ�BM�B=qB�B
�B
ÖB
�B
�PB
.B	�B	�`B	�B	�qB	��B	��B	x�B	hsB	dZB	H�B	,B	�B	  B�B�B�B	VB	$�B	0!B	<jB	?}B	D�B	D�B	D�B	G�B	M�B	R�B	YB	\)B	]/B	\)B	]/B	`BB	]/B	aHB	ZB	D�B	49B	,B	,B	2-B	@�B	(�B	"�B	=qB	e`B	m�B	k�B	� B	�B	�DB	��B	��B	��B	��B	��B	�B	�;B	�HB	�5B	�5B	�BB	�fB	�B	�B	�B	�B	�B	��B	�B	�;B	�#B	�B	�#B	�5B	�sB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
B
B
JB
PB
VB
hB
�B
�B
�B
�B
{B
�B
�B
hB
hB
�B
�B
�B
�B
�B
�B
�B
�B
"�B
$�B
"�B
�B
�B
�B
!�B
#�B
.B
2-B
/B
-B
,B
,B
'�B
#�B
!�B
%�B
%�B
$�B
#�B
#�B
$�B
&�B
%�B
!�B
�B
�B
�B
�B
�B
�B
oB
\B
uB
{B
bB

=B
	7B

=B
	7B
	7B
%B
1B

=B
JB
VB
VB
%B
%B
B	��B
B	��B	��B	��B
B

=B
PB
\B
PB
JB
VB
	7B
%B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�mB	�TB	�fB	�B	�;B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�5B	�/B	�HB	�TB	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�sB	�yB	�mB	�yB	�B	�sB	�B	�fB	�mB	�fB	�mB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
	7B

=B
1B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B
  B	��B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
%B
1B
1B
1B

=B
DB
JB
VB
PB
PB
VB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
\B
\B
VB
bB
bB
bB
uB
oB
hB
bB
hB
bB
bB
uB
uB
uB
{B
�B
 �B
"�B
+B
2-B
8RB
<jB
J�B
K�B
S�B
ZB
`BB
cTB
hsB
k�B
o�B
r�B
v�B
z�B
~�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<�t�<e`B<#�
<���=t�<�`B<�j<�9X<���<D��<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<���<u<#�
<#�
<#�
<49X<#�
<#�
<�9X<�1<�o<#�
<#�
<u<#�
<#�
<�C�<�o<#�
<e`B<���<u<���<#�
<D��<��
<�/=+=\)<���=�w=aG�<��<u<���<�/<���<�C�<�/<�t�<�t�=o<�`B<���<���<�j<��<���<u<��
<e`B<�t�<T��<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<�C�<���<D��<#�
<#�
<#�
<�o<���<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.4 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250172012011312501720120113125018  AO  ARGQ                                                                        20111205112828  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205112828  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125018  IP                  G�O�G�O�G�O�                