CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-03-06T12:35:37Z creation;2018-03-06T12:35:40Z conversion to V3.1;2019-12-23T06:25:53Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180306123537  20200120021522  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ,A   JA  I2_0675_044                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�QXY <�1   @�QY����@6��
=p��b�'RT`�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�C3Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ D�|�D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�33@���A��A&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B��B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���Bę�B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCL�CffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvL�CxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�&fC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�&fC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�&fC�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D3D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D�� D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D�ɚD��D�L�D���D���D��D�L�D�D���D��D�L�DÌ�D���D��D�L�DČ�D���D��D�L�DŌ�D���D��D�L�Dƌ�D���D��D�P Dǌ�D���D��D�L�DȌ�D���D��D�L�DɌ�D���D��D�L�Dʌ�D���D��D�L�Dˌ�D���D��D�L�Ď�D���D��D�L�D͌�D���D��D�L�DΌ�D���D��D�L�Dό�D���D��D�L�DЌ�D���D��D�L�Dь�D���D��D�L�DҌ�D���D��D�L�Dӌ�D���D��D�L�DԌ�D���D��D�L�DՌ�D���D��D�L�D֌�D���D��D�L�D׌�D���D��D�L�D،�D���D��D�L�Dٌ�D���D��D�L�Dڌ�D���D��D�L�Dۉ�D���D��D�L�D܌�D���D��D�L�D݌�D���D��D�L�Dތ�D���D��D�L�Dߌ�D���D��D�L�D���D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D���D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�?}A�S�A�VA�VA�VA�XA�XA�XA�ZA�ZA�\)A�\)A�\)A�^5A�^5A�^5A�\)A�\)A�^5A�^5A�`BA�`BA�^5A�\)A�VA�VA�Q�A�G�A�?}A�A�A�;dA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�5?A�33A�/A��A��A��TA���A���A��A���A�Q�A�?}A��jA���A�Q�A��A���A���A��PA��A���A�x�A�=qA���A�1'A�l�A�5?A�|�A�S�A�l�A��!A�+A���A�$�A�+A���A�t�A�{A���A��A�"�A�O�A��yA��9A���A��!A�p�A��/A�\)A��A�S�A�A��A��A�=qA��9A��A��DA�VA���A���A�M�A��A�
=A}�TA{S�AxjAwK�Av�uAt�\As��AqG�Am�Ak+Ai\)AehsAa�;A^�/A\�AZ(�AVI�AS�TAR�jAQ��AQ�AP��AN  ALA�ALVAKS�AJ(�AGx�AD�ABbNA@ffA>�/A<�uA:�+A7ƨA6�A5VA4-A3��A2�\A1\)A/�#A.��A-�wA-&�A,�A+\)A*Q�A)33A(��A'�#A&�/A%��A#�;A#O�A"�\A �9A�AXA-A"�A�FA��A�PA/AC�AS�AA�jAbAp�At�A�A��An�A��A
�!A	��A	l�A=qA�wAQ�A�7A&�A�9AE�A�;A�AC�An�AXA ��@��
@���@���@���@�Q�@�ƨ@�o@��\@��@�o@���@��u@�@�x�@��@�~�@��T@�`B@�/@���@�u@땁@�v�@� �@�t�@�@�A�@��@�o@�?}@��;@�t�@ޗ�@��@�@׶F@��H@Ցh@ԋD@�(�@���@���@��#@���@Л�@�Z@�l�@�^5@�r�@�5?@���@�z�@�+@�M�@ź^@��`@��m@���@���@�O�@�A�@��@�&�@��P@�
=@�{@�p�@�Ĝ@�1'@��F@�+@��R@�ff@�-@���@���@�&�@��u@��m@���@��T@��^@��7@�`B@�{@��^@�G�@�7L@���@�%@�V@�Ĝ@�Ĝ@��j@��@��@�33@��y@�ȴ@�ff@���@���@�I�@��@�o@���@�5?@���@���@��@��/@�r�@���@��@�5?@��@�O�@��/@�A�@��@�dZ@�~�@���@�X@���@��D@�bN@�A�@���@�\)@���@�v�@�^5@�V@�$�@��h@�?}@�Q�@��m@��;@���@��w@���@�S�@�o@��R@�n�@�@�G�@�`B@�`B@�G�@�/@�V@���@��@���@�Z@�A�@�1@��w@�C�@��@�ȴ@�n�@�^5@�M�@�$�@���@�X@��@�Q�@�A�@�9X@�(�@��@�b@���@��m@���@�t�@�"�@�@�
=@�o@�o@�
=@��H@��R@��\@�=q@��#@�p�@�X@�7L@��@�%@��`@���@�I�@�b@��@���@�ƨ@��F@���@�l�@�K�@�"�@��@��@�o@�@��@��H@��@��R@��!@���@��\@��@��@�@��h@�hs@�`B@�`B@�G�@�&�@���@��@�Ĝ@�z�@�b@���@���@�\)@�C�@��R@��+@�V@���@��-@�hs@�7L@��@��`@�j@��@��@��
@���@�|�@�dZ@�o@��+@��@��T@���@���@�x�@�hs@�O�@��@�/@�?}@��@��/@�%@��`@���@��9@��u@�I�@�b@���@�\)@�;d@�C�@�\)@�K�@�S�@�;d@��@��H@���@�n�@�V@�-@��-@�p�@�O�@�G�@�7L@��/@�j@�bN@�Z@�1'@�w@;d@~�@~��@~v�@~V@~E�@~{@~{@~@}�@}�T@}��@}��@}p�@|��@{��@{�F@{t�@{"�@{@z�H@z�!@z=q@y��@yG�@x�`@xQ�@w��@w�w@w�@w�@w��@w|�@wl�@wK�@w
=@v�@vȴ@v$�@u�@u��@u?}@t��@tZ@tI�@t(�@s�
@st�@sdZ@sdZ@sdZ@s33@s@r�@r��@r~�@rn�@r=q@q�@q��@p��@pbN@pb@o��@o�P@o+@nff@m@m/@l�@l1@k"�@j��@i��@iX@hĜ@h1'@g��@f�@fff@f@e�T@e��@e?}@d��@dj@c��@c�m@c�
@c��@c"�@b��@b�\@b�@a��@a&�@`��@`��@`��@`b@_l�@_+@^��@^�y@^ȴ@^ff@]@]�-@]�@\��@\��@\�D@\9X@[�
@[t�@[S�@[33@["�@Z��@Zn�@Y�@Y��@Y��@Yx�@Yx�@Yx�@YG�@Y�@X�u@XA�@W�;@W�P@W+@V��@Vv�@Vff@V5?@U��@Up�@U`B@U?}@U/@T��@Tz�@T1@Sƨ@S��@St�@R�@R-@Q�@Q�#@Q��@Q�^@Q�^@Q��@Q��@QX@Pr�@P  @O�P@O\)@O+@O;d@O;d@O�@Nȴ@NV@M�T@M�h@MO�@L�/@L�D@L1@K��@KS�@Ko@J�H@J��@J~�@J~�@J~�@J~�@J^5@I�@I�#@I�#@I��@I��@IX@IG�@I7L@I7L@H��@H�9@H�@HQ�@G�@G\)@G+@G�@F�y@F��@Fff@E�@E�-@EO�@E?}@E?}@E�@D�@Dj@C��@C�F@C��@C�@CS�@C"�@B��@B^5@A�#@A��@Ahs@A&�@A%@@��@@�`@@��@@��@@Ĝ@@Ĝ@@Ĝ@@Ĝ@@Ĝ@@Ĝ@@Ĝ@@r�@@b@?�w@>��@>@=�h@=/@=/@=/@=�@<�/@<z�@<(�@<�@;�
@;�F@;dZ@;"�@:�!@:~�@:J@9��@9x�@9�@8�@8Q�@8 �@8  @7�;@7�w@7�@7��@7�P@7;d@7�@6�y@6�R@6v�@6V@6{@5�@5/@4�@4�j@4��@4j@4(�@3��@3@2�\@2M�@1��@1&�@0�9@0��@0bN@0 �@/�P@/l�@/\)@/;d@/�@.��@.�+@.E�@.5?@-@-O�@-/@,9X@+��@+�m@+��@+33@*��@*M�@*�@)�@)��@)x�@)X@)G�@)&�@(��@(��@(Ĝ@(r�@(Q�@(b@'|�@';d@'
=@&�y@&�y@&��@&�@&v�@&$�@&@%��@%��@%p�@%?}@$�@$Z@$I�@$I�@$I�@$9X@$(�@$1@#��@#�
@#�F@#��@#��@#t�@#dZ@#C�@#33@#@"�H@"~�@"=q@"�@"J@!��@!��@!��@!��@!��@!hs@!�@ �`@ Ĝ@ r�@ bN@ A�@ A�@ 1'@�;@�@��@\)@�y@��@ff@E�@5?@$�@{@@�@��@�-@��@�@`B@?}@�j@j@9X@9X@9X@(�@(�@(�@��@�
@��@@@@�H@��@�\@^5@-@��@�@��@Q�@A�@1'@1'@1'@  @�;@��@l�@�@ȴ@��@�+@v�@V@E�@{@��@�-@��@�@O�@�@�/@�j@j@9X@�
@��@dZ@S�@S�@"�@�H@��@��@�!@~�@=q@��@��@��@7L@��@��@�@r�@A�@�@��@;d@
=@�y@��@5?@$�@@@��@��@�@��@�D@z�@9X@��@��@�@�@t�@S�@33@"�@o@
��@
��@
^5@	�@	�^@	�7@	hs@	7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�?}A�S�A�VA�VA�VA�XA�XA�XA�ZA�ZA�\)A�\)A�\)A�^5A�^5A�^5A�\)A�\)A�^5A�^5A�`BA�`BA�^5A�\)A�VA�VA�Q�A�G�A�?}A�A�A�;dA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�7LA�5?A�33A�/A��A��A��TA���A���A��A���A�Q�A�?}A��jA���A�Q�A��A���A���A��PA��A���A�x�A�=qA���A�1'A�l�A�5?A�|�A�S�A�l�A��!A�+A���A�$�A�+A���A�t�A�{A���A��A�"�A�O�A��yA��9A���A��!A�p�A��/A�\)A��A�S�A�A��A��A�=qA��9A��A��DA�VA���A���A�M�A��A�
=A}�TA{S�AxjAwK�Av�uAt�\As��AqG�Am�Ak+Ai\)AehsAa�;A^�/A\�AZ(�AVI�AS�TAR�jAQ��AQ�AP��AN  ALA�ALVAKS�AJ(�AGx�AD�ABbNA@ffA>�/A<�uA:�+A7ƨA6�A5VA4-A3��A2�\A1\)A/�#A.��A-�wA-&�A,�A+\)A*Q�A)33A(��A'�#A&�/A%��A#�;A#O�A"�\A �9A�AXA-A"�A�FA��A�PA/AC�AS�AA�jAbAp�At�A�A��An�A��A
�!A	��A	l�A=qA�wAQ�A�7A&�A�9AE�A�;A�AC�An�AXA ��@��
@���@���@���@�Q�@�ƨ@�o@��\@��@�o@���@��u@�@�x�@��@�~�@��T@�`B@�/@���@�u@땁@�v�@� �@�t�@�@�A�@��@�o@�?}@��;@�t�@ޗ�@��@�@׶F@��H@Ցh@ԋD@�(�@���@���@��#@���@Л�@�Z@�l�@�^5@�r�@�5?@���@�z�@�+@�M�@ź^@��`@��m@���@���@�O�@�A�@��@�&�@��P@�
=@�{@�p�@�Ĝ@�1'@��F@�+@��R@�ff@�-@���@���@�&�@��u@��m@���@��T@��^@��7@�`B@�{@��^@�G�@�7L@���@�%@�V@�Ĝ@�Ĝ@��j@��@��@�33@��y@�ȴ@�ff@���@���@�I�@��@�o@���@�5?@���@���@��@��/@�r�@���@��@�5?@��@�O�@��/@�A�@��@�dZ@�~�@���@�X@���@��D@�bN@�A�@���@�\)@���@�v�@�^5@�V@�$�@��h@�?}@�Q�@��m@��;@���@��w@���@�S�@�o@��R@�n�@�@�G�@�`B@�`B@�G�@�/@�V@���@��@���@�Z@�A�@�1@��w@�C�@��@�ȴ@�n�@�^5@�M�@�$�@���@�X@��@�Q�@�A�@�9X@�(�@��@�b@���@��m@���@�t�@�"�@�@�
=@�o@�o@�
=@��H@��R@��\@�=q@��#@�p�@�X@�7L@��@�%@��`@���@�I�@�b@��@���@�ƨ@��F@���@�l�@�K�@�"�@��@��@�o@�@��@��H@��@��R@��!@���@��\@��@��@�@��h@�hs@�`B@�`B@�G�@�&�@���@��@�Ĝ@�z�@�b@���@���@�\)@�C�@��R@��+@�V@���@��-@�hs@�7L@��@��`@�j@��@��@��
@���@�|�@�dZ@�o@��+@��@��T@���@���@�x�@�hs@�O�@��@�/@�?}@��@��/@�%@��`@���@��9@��u@�I�@�b@���@�\)@�;d@�C�@�\)@�K�@�S�@�;d@��@��H@���@�n�@�V@�-@��-@�p�@�O�@�G�@�7L@��/@�j@�bN@�Z@�1'@�w@;d@~�@~��@~v�@~V@~E�@~{@~{@~@}�@}�T@}��@}��@}p�@|��@{��@{�F@{t�@{"�@{@z�H@z�!@z=q@y��@yG�@x�`@xQ�@w��@w�w@w�@w�@w��@w|�@wl�@wK�@w
=@v�@vȴ@v$�@u�@u��@u?}@t��@tZ@tI�@t(�@s�
@st�@sdZ@sdZ@sdZ@s33@s@r�@r��@r~�@rn�@r=q@q�@q��@p��@pbN@pb@o��@o�P@o+@nff@m@m/@l�@l1@k"�@j��@i��@iX@hĜ@h1'@g��@f�@fff@f@e�T@e��@e?}@d��@dj@c��@c�m@c�
@c��@c"�@b��@b�\@b�@a��@a&�@`��@`��@`��@`b@_l�@_+@^��@^�y@^ȴ@^ff@]@]�-@]�@\��@\��@\�D@\9X@[�
@[t�@[S�@[33@["�@Z��@Zn�@Y�@Y��@Y��@Yx�@Yx�@Yx�@YG�@Y�@X�u@XA�@W�;@W�P@W+@V��@Vv�@Vff@V5?@U��@Up�@U`B@U?}@U/@T��@Tz�@T1@Sƨ@S��@St�@R�@R-@Q�@Q�#@Q��@Q�^@Q�^@Q��@Q��@QX@Pr�@P  @O�P@O\)@O+@O;d@O;d@O�@Nȴ@NV@M�T@M�h@MO�@L�/@L�D@L1@K��@KS�@Ko@J�H@J��@J~�@J~�@J~�@J~�@J^5@I�@I�#@I�#@I��@I��@IX@IG�@I7L@I7L@H��@H�9@H�@HQ�@G�@G\)@G+@G�@F�y@F��@Fff@E�@E�-@EO�@E?}@E?}@E�@D�@Dj@C��@C�F@C��@C�@CS�@C"�@B��@B^5@A�#@A��@Ahs@A&�@A%@@��@@�`@@��@@��@@Ĝ@@Ĝ@@Ĝ@@Ĝ@@Ĝ@@Ĝ@@Ĝ@@r�@@b@?�w@>��@>@=�h@=/@=/@=/@=�@<�/@<z�@<(�@<�@;�
@;�F@;dZ@;"�@:�!@:~�@:J@9��@9x�@9�@8�@8Q�@8 �@8  @7�;@7�w@7�@7��@7�P@7;d@7�@6�y@6�R@6v�@6V@6{@5�@5/@4�@4�j@4��@4j@4(�@3��@3@2�\@2M�@1��@1&�@0�9@0��@0bN@0 �@/�P@/l�@/\)@/;d@/�@.��@.�+@.E�@.5?@-@-O�@-/@,9X@+��@+�m@+��@+33@*��@*M�@*�@)�@)��@)x�@)X@)G�@)&�@(��@(��@(Ĝ@(r�@(Q�@(b@'|�@';d@'
=@&�y@&�y@&��@&�@&v�@&$�@&@%��@%��@%p�@%?}@$�@$Z@$I�@$I�@$I�@$9X@$(�@$1@#��@#�
@#�F@#��@#��@#t�@#dZ@#C�@#33@#@"�H@"~�@"=q@"�@"J@!��@!��@!��@!��@!��@!hs@!�@ �`@ Ĝ@ r�@ bN@ A�@ A�@ 1'@�;@�@��@\)@�y@��@ff@E�@5?@$�@{@@�@��@�-@��@�@`B@?}@�j@j@9X@9X@9X@(�@(�@(�@��@�
@��@@@@�H@��@�\@^5@-@��@�@��@Q�@A�@1'@1'@1'@  @�;@��@l�@�@ȴ@��@�+@v�@V@E�@{@��@�-@��@�@O�@�@�/@�j@j@9X@�
@��@dZ@S�@S�@"�@�H@��@��@�!@~�@=q@��@��@��@7L@��@��@�@r�@A�@�@��@;d@
=@�y@��@5?@$�@@@��@��@�@��@�D@z�@9X@��@��@�@�@t�@S�@33@"�@o@
��@
��@
^5@	�@	�^@	�7@	hs@	7L111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B!�B �B �B!�B!�B �B!�B!�B!�B!�B!�B!�B!�B�B!�B!�B!�B!�B!�B!�B!�B"�B#�B#�B%�B%�B&�B(�B)�B)�B+B+B+B+B+B+B+B,B,B,B,B,B,B,B,B,B,B.B6FBE�B]/B�B�VB�PB�uB��B��B��B�VB�Bk�B`BBXB>wB1'B(�B$�B�B�B��B�fB�#B��B��B��BĜB��B�3B��B��B�bB�7B�BgmBE�B/B�B
��B
�fB
�NB
�mB
�B
�yB
�;B
�
B
��B
��B
�'B
�B
��B
��B
��B
��B
��B
�=B
}�B
n�B
A�B
 �B
�B	��B	�B	�fB	�B	ɺB	�FB	��B	�=B	v�B	\)B	6FB	�B��B�B��BB�RB�-B�?B�?B��B��B�9B�RB�?B��B�bB�DB�%B~�Bx�Bt�Bm�BdZBdZBn�Bn�Bo�Bm�Bk�BgmBdZBbNBaHB_;B[#BYBYBXBVBS�BQ�BO�BN�BK�BI�BH�BF�BA�B@�B>wB;dB7LB7LB49B49B49B2-B0!B/B(�B'�B&�B.B1'B2-B2-B0!B.B+B&�B%�B%�B$�B#�B"�B!�B!�B �B"�B!�B!�B#�B#�B#�B$�B%�B%�B&�B&�B'�B(�B)�B+B,B,B,B,B,B,B,B,B,B,B)�B+B)�B(�B(�B+B(�B'�B&�B(�B(�B(�B(�B)�B+B+B+B,B+B,B+B+B,B,B-B/B0!B0!B2-B2-B2-B49B6FB8RB8RB9XB:^B;dB@�BC�BC�BG�BI�BJ�BL�BM�BO�BQ�BR�BT�BYB]/BgmBr�Bv�Bx�Bx�By�Bz�B~�B�7B�PB�bB�hB�oB��B��B��B��B��B��B��B��B��B��B��B�B�3B�LB�jB�}B��BBÖBŢBǮBǮBɺB��B��B�B�5B�;B�HB�TB�mB�B�B��B��B��B��B��B��B	  B	B		7B	JB	PB	PB	\B	{B	�B	�B	!�B	#�B	$�B	%�B	(�B	,B	0!B	49B	6FB	:^B	?}B	B�B	D�B	D�B	F�B	F�B	G�B	I�B	L�B	P�B	Q�B	S�B	XB	_;B	dZB	gmB	jB	k�B	l�B	p�B	t�B	x�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�+B	�DB	�PB	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�-B	�-B	�3B	�3B	�9B	�9B	�9B	�RB	�XB	�^B	�dB	�jB	�jB	�jB	�qB	�wB	�wB	�}B	�}B	��B	ÖB	ĜB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�;B	�BB	�ZB	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
+B
1B
1B
1B
	7B

=B

=B

=B

=B
DB
JB
JB
JB
JB
PB
PB
PB
PB
VB
VB
VB
VB
\B
bB
bB
hB
hB
hB
hB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
+B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
.B
.B
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B!�B �B �B!�B!�B �B!�B!�B!�B!�B!�B!�B!�B�B!�B!�B!�B!�B!�B!�B!�B"�B#�B#�B%�B%�B&�B(�B)�B)�B*�B*�B*�B*�B*�B*�B*�B+�B+�B+�B+�B+�B+�B+�B+�B+�B+�B-�B6+BEmB\�B��B�"B�6B�@B��B��B�gB�"B��BkkB`'BW�B>]B0�B(�B$�B�BSB��B�LB��B͹B��B˒BāB�UB�B��B�yB�.B�B��Bg8BE�B.�B�B
��B
�LB
�4B
�8B
�kB
�DB
�!B
��B
ϫB
�iB
�B
��B
��B
��B
��B
��B
�qB
�	B
}�B
ncB
AoB
 �B
MB	��B	�]B	�2B	��B	ɠB	�+B	��B	�#B	v�B	\B	6B	qB��B�qBϫB�[B�B�B�B�B��B��B�B�B�%B��B�.B�)B��B~�Bx�Bt�Bm]Bd&Bd@BncBncBoiBm]BkkBg8Bd&BbBa-B_BZ�BX�BX�BW�BU�BS�BQ�BO�BN�BK�BI�BH�BFtBAUB@OB>BB;0B7B7B4B4B4B1�B/�B.�B(�B'�B&�B-�B0�B2B1�B/�B-�B*�B&�B%�B%�B$�B#�B"�B!�B!�B �B"�B!�B!�B#�B#�B#�B$�B%�B%�B&�B&�B'�B(�B)�B*�B+�B+�B+�B+�B+�B+�B+�B+�B+�B+�B)�B*�B)�B(�B(�B*�B(�B'�B&�B(�B(�B(�B(�B)�B*�B*�B*�B+�B*�B+�B*�B*�B+�B+�B,�B.�B/�B/�B1�B1�B1�B4B6B8B8B9$B:*B;0B@OBCaBCaBG�BI�BJ�BL�BM�BO�BQ�BR�BT�BX�B]Bg8Br|Bv�Bx�Bx�By�Bz�B~�B�B�B�.B�4B�:B�MB�YB�_B�kB�kB�WB��B��B��B��B��B��B��B�B�6B�HB�OB�[B�aBňBǔB�zBɠB˒BөB��B�B�B�B� B�8B�KB�oB��B��B��B��B��B��B��B	�B		B	B	B	B	(B	FB	SB	�B	!�B	#�B	$�B	%�B	(�B	+�B	/�B	4B	6B	:*B	?HB	B[B	DgB	DgB	FtB	FtB	G�B	I�B	L�B	P�B	Q�B	S�B	W�B	_!B	d&B	g8B	jKB	kQB	lWB	p�B	t�B	x�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�B	�(B	�.B	�4B	�@B	�SB	�YB	�YB	�_B	�eB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�*B	�0B	�6B	�6B	�B	�<B	�BB	�BB	�.B	�HB	�UB	�aB	�gB	�tB	�YB	�zB	ʦB	ˬB	˒B	͟B	ϫB	бB	ѷB	ѝB	ңB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�&B	�,B	�2B	�DB	�KB	�QB	�WB	�WB	�WB	�CB	�cB	�IB	�iB	�iB	�iB	�oB	�vB	�vB	�|B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

#B

	B

	B

	B
B
B
B
B
B
B
B
B
B
B
"B
"B
B
(B
.B
.B
4B
4B
4B
B
 B
@B
@B
@B
FB
aB
FB
MB
MB
SB
YB
YB
EB
eB
eB
eB
eB
eB
B
QB
kB
qB
qB
qB
qB
xB
xB
xB
xB
xB
~B
~B
~B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
-�B
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
0�B
0�B
0�B
2B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3B
2�B
2�B
2�B
2�B
2�B
4B
4B
4B
3�B
4B
3�B
5%B
5%B
5%B
6+B
6B
6B
5�B
5�B
6B
6B
7B
7B
8B
8B
8B
8B
8B
8B
9$B
9>B
9$B
9$B
9$B
:*B
:*B
:DB
;0B
;0B
<PB
<6B
<PB
<6B
<6B
<6B
<B
<6B
<6B
<6B
<6B
<6B
<6B
<6B
=<B
=<B
=<B
>BB
?HB
?HB
?HB
?HB
?HB
?HB
@4B
@OB
AUB
AUB
AUB
AUB
AUB
AUB
B[B
B[B
CGB
CaB
CaB
CaB
D�B
D�B
D�B
DgB
EmB
EmB
EmB
EmB
EmB
EmB
EmB
EmB
EmB
FtB
EmB
FtB
FtB
G_B
GzB
GzB
G_B
G_B
H�B
H�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
KxB
K�B
L~B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[	B
Z�B
Z�B
Z�B
Z�B
\B
[�B
[�B
\B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
^B
^B
^B
^B
^B
^B
_!B
_B
_B
_B
_B
_B
_B
_B
`B
`B
`B
aB
`B
`B
aB
aB
aB
aB
aB
bB
bB
c:B
c B
cB
c B
c B
c B
c B
c B
d&B
d&B
dB
d&B
e,B
eB
e,B
eFB
e,B
e,B
e,B
e,B
e,B
e,B
f2B
f2B
f2B
f2B
f2B
g8B
g8B
g8B
h>B
h>B
h>B
h>B
h>B
h>B
h>B
h>B
h>B
iDB
i_B
i*B
iDB
jKB
jeB
jKB
jKB
jKB
kQB
kQB
kQB
lWB
lWB
lWB
lWB
m]B
mCB
m]B
m]B
ncB
ncB
oiB
oiB
oiB
oiB
oiB
poB
poB
pUB
pUB
poB
poB
qvB
q�B
qvB
qvB
qvB
qvB
raB
r|B
r|B
r|B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.4(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201803130112582018031301125820180313011258201804060311172018040603111720180406031117JA  ARFMdecpA19c                                                                20180306213523  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180306123537  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180306123538  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180306123539  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180306123539  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180306123539  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180306123540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180306123540  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180306123540  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180306123540                      G�O�G�O�G�O�                JA  ARUP                                                                        20180306125455                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180306153715  CV  JULD            G�O�G�O�F�                JM  ARCAJMQC2.0                                                                 20180312161258  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180312161258  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180405181117  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021522                      G�O�G�O�G�O�                