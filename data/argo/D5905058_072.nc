CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-07-23T07:09:18Z creation;2018-07-23T07:09:26Z conversion to V3.1;2019-12-23T06:19:15Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180723070918  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               HA   JA  I2_0675_072                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�o?UUU�1   @�o@`� @9D��*0�c)�^5?}1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�|�D�� D�  D�@ Dˀ D�� D�  D�@ D̃3D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�<�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @333@���@���AffA&ffAFffAfffA�33A�33A�33A�33A�33A�33A�33A�33B  B	��B��B��B!��B)��B1��B9��BA��BI��BQ��BY��Ba��Bi��Bq��By��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C ffCffCffCffCffC
ffCffCffCffCffCffCffCffCffCffCffC ffC"ffC$ffC&ffC(ffC*ffC,ffC.ffC0ffC2ffC4ffC6ffC8ffC:ffC<ffC>ffC@ffCBffCDffCFffCHffCJffCLffCNffCPffCRffCTffCVffCXffCZffC\ffC^ffC`ffCbffCdffCfffChffCjffClffCnffCpffCrffCtffCvffCxffCzffC|ffC~ffC�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�@ C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33C�33D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+3D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE  DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[3D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D�ɚD��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�I�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D�D���D��D�L�DÌ�D���D��D�L�DČ�D���D��D�L�DŌ�D���D��D�L�Dƌ�D���D��D�L�Dǌ�D���D��D�L�DȌ�D���D��D�L�DɌ�D���D��D�L�Dʉ�D���D��D�L�Dˌ�D���D��D�L�D̐ D���D��D�L�D͌�D���D��D�L�DΌ�D���D��D�L�Dό�D���D��D�L�DЌ�D���D��D�L�Dь�D���D��D�L�DҌ�D���D��D�L�Dӌ�D���D��D�L�DԌ�D���D��D�L�DՌ�D���D��D�L�D֌�D���D��D�L�D׌�D���D��D�L�D،�D���D��D�L�Dٌ�D���D��D�L�Dڌ�D���D��D�L�Dی�D���D��D�L�D܌�D���D��D�L�D݌�D���D��D�L�Dތ�D���D��D�L�Dߌ�D���D��D�L�D���D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D�� D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D���D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D��D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D��D�L�D���D���D� D�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�M�A�O�A�Q�A�Q�A�ZA�ZA�XA�XA�?}A�9XA�&�A�/A�(�A�"�A��A�1A��`A�;dAŬAÍPA�bNA��jA�jA�~�A�|�A���A�=qA�?}A�-A��DA��A�&�A��A�n�A��A�E�A�1A��^A�r�A�A�+A��^A�\)A�A��A��^A�dZA�M�A�x�A��
A���A��jA�|�A��DA�1'A�`BA�+A�A�A��A��wA�(�A�1'A�/A�n�A�/A�ƨA��^A��A�{A��A��yA��hA�33A��A��7A�XA�oA�9XA�x�A��
A���A���A���A�G�A��uA��A�I�A��!A�|�A��A���A��jA��FA��`A�ȴA���A���A��A�v�A��TA��uA�oA�`BA�{A��uA�JA��A���A��A�VA~��A}\)Az��Ax�+Awp�AuhsAst�Aq|�Ao/Am/Ajr�AgC�AeVAcXAa�PA_�wA^A[�AY�AX�AW&�AT��AS�
AS+AR�APM�AOdZAN�RAM�wAL=qAK%AJ{AHVAGG�AFAD�!AC��AB�ABr�AB�/AB�DAA�wA?�
A>v�A=dZA;��A:�/A:��A9��A9hsA8�jA7l�A6��A4�DA2��A1�wA0��A/��A.�\A-"�A+�
A+;dA*�A*jA)C�A(bNA'C�A%p�A$��A#�A#O�A"�+A"=qA!�A ��A7LA��A�+AM�A�At�A��AZAC�AZA��A�A��A��A�\A�A��AA/AȴAVA  AdZAE�A��AȴA��A�A
ȴA
A�A�yA�-AG�AffA�FA/A��A�A�+A��A|�AA ��A n�@��@��@���@�-@���@�33@�7L@�ȴ@�/@���@�$�@�G�@띲@�R@�^@�;d@��@��@�ƨ@�;d@���@�E�@�hs@ߍP@�v�@�X@�\)@��@�@�?}@���@�Ĝ@ץ�@��T@���@ӥ�@ёh@�z�@���@���@͙�@̃@�1'@��m@�|�@�S�@ʏ\@�=q@�{@�G�@ȣ�@ț�@ȓu@�(�@�S�@�5?@���@��@Å@��@��@���@��9@� �@�33@�~�@�@�%@� �@�K�@��-@��P@�;d@�$�@�&�@���@�r�@���@�t�@�33@�@��@���@�%@�I�@��;@�\)@�J@��@���@�t�@��R@��@���@��@�7L@��9@��u@�(�@���@�t�@���@�+@��R@�ff@�@��7@�V@��u@��w@�;d@��@���@��+@�=q@�@�@��h@�`B@��@��/@�bN@�  @���@��@�V@��@���@��7@�p�@�`B@�X@�`B@�/@��D@�1'@�  @��m@�b@�1@�+@��@���@���@���@�=q@��-@�O�@�/@�/@�/@�7L@�?}@�X@�V@��@�z�@�z�@�Q�@� �@��w@���@�33@��\@�ff@�5?@��h@�hs@�G�@��`@�r�@���@��F@���@�C�@�@�
=@�
=@�@���@�ȴ@���@��@���@�7L@��@��9@�bN@��@� �@�b@��;@�ƨ@��w@�t�@�|�@���@�dZ@�33@��!@�-@�$�@�E�@�{@���@���@���@���@���@��-@���@��@�x�@�hs@�G�@�/@�7L@���@��j@��j@��@��9@���@��@��@��m@���@��P@�l�@�;d@��@��@�o@�
=@��@���@�V@��@�@��7@�O�@�7L@�/@���@�r�@�I�@� �@�ƨ@�dZ@�"�@�"�@��@���@���@���@���@���@�V@�$�@���@�x�@�&�@��`@��9@�Z@�(�@�(�@���@�1@�b@��@�1@��m@��@���@���@���@�K�@���@�n�@�^5@���@�X@�X@�`B@�?}@�?}@�/@�%@��u@�9X@�1@�  @�@�P@\)@�P@�;@�  @� �@�;@\)@
=@~��@~ȴ@~E�@~{@}��@|��@|��@|�D@|z�@|j@|9X@|1@{�
@{�@{S�@z�@z��@y�#@y��@yX@y%@xĜ@x��@xQ�@xb@w�@w��@w�@wl�@w�@vv�@u�T@u�-@t��@tz�@t��@tj@s�
@s�F@s��@s�@s33@r=q@rJ@q�@q��@p��@p��@p  @p �@o�P@o\)@o;d@nȴ@n$�@m�@m��@m�@l�/@l��@lz�@l�@k�m@k�
@k�F@jM�@i�@i%@h�9@h �@g��@g;d@f��@fV@f{@e�@e@e�@d��@d�@d��@dZ@c�
@c��@co@b��@b�!@b�!@b~�@a��@`�`@_�;@_l�@_�@^��@^E�@^{@^@^@^@^@]�@]��@]`B@]V@\��@\j@[�m@[��@[dZ@[dZ@[C�@[o@Z��@Z��@Z^5@Z-@Y��@Y��@Y�7@YX@Y&�@Y%@X�9@X�u@Xr�@X �@W�P@V��@Vȴ@V��@V�+@VV@VE�@V{@U�T@U�-@U�@T��@T�D@T(�@S�@R�@RM�@Q��@QX@P��@P�u@O��@O��@O��@Ol�@O+@N�@Nȴ@N�R@N��@Nv�@NE�@N$�@Mp�@M?}@M�@MV@L�j@L�D@L9X@K��@K��@K�@KC�@J�@J�\@J=q@I��@I�7@I�@HbN@Hb@G�@G��@G��@Gl�@G\)@Fȴ@F�+@E@E�@DZ@C�
@Cƨ@C�@CC�@C"�@B�H@B~�@B=q@A��@A&�@A%@@��@@��@@�9@@r�@@1'@@  @?l�@?�@>��@>��@>v�@>V@>5?@=�@=��@=@=�h@=?}@=�@=�@=�@<�@<��@<z�@<1@;t�@;33@;"�@;"�@;o@:��@:M�@:-@:�@9�@9��@9x�@8��@81'@7�@7�w@7�@7�P@7|�@7|�@7K�@7
=@6�R@6V@6{@5@5p�@5`B@5?}@5�@4��@4�/@4��@4�D@4Z@41@3�m@3t�@3C�@333@3@2�H@2��@2�\@2^5@2=q@1��@1�#@1�^@1��@1x�@1X@1�@0�9@0�u@0�u@0r�@0b@/�P@/K�@.��@.E�@-��@-�-@-��@-`B@-?}@,�@,z�@,(�@+��@+C�@*�H@*��@*n�@*^5@*=q@*J@)��@)hs@(�`@(�@( �@(  @'��@'l�@'+@&�y@&�R@&��@&v�@&{@%�@%�@$��@$�@$j@$�@#��@#33@#@"n�@"J@!�^@!�7@!hs@!G�@!&�@!&�@!&�@!�@!%@ ��@ ��@  �@�w@�P@l�@l�@K�@;d@+@��@�@��@v�@v�@ff@$�@@��@p�@��@�j@�@z�@I�@Z@Z@9X@�@�m@ƨ@ƨ@��@�@t�@t�@dZ@dZ@S�@S�@C�@33@"�@�@�H@��@��@��@=q@��@�#@��@��@x�@X@7L@�@�@%@��@��@�9@�u@Q�@  @�@|�@l�@;d@�@�@v�@��@��@p�@?}@�/@��@�D@j@Z@Z@I�@9X@9X@(�@1@�@�@~�@M�@�@��@x�@X@7L@��@Ĝ@�@A�@b@�;@�w@�P@\)@;d@�@�@ff@V@V@E�@5?@{@�@@@@�-@�h@�@`B@O�@?}@?}@/@�@�@�/@�j@�D@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�M�A�O�A�Q�A�Q�A�ZA�ZA�XA�XA�?}A�9XA�&�A�/A�(�A�"�A��A�1A��`A�;dAŬAÍPA�bNA��jA�jA�~�A�|�A���A�=qA�?}A�-A��DA��A�&�A��A�n�A��A�E�A�1A��^A�r�A�A�+A��^A�\)A�A��A��^A�dZA�M�A�x�A��
A���A��jA�|�A��DA�1'A�`BA�+A�A�A��A��wA�(�A�1'A�/A�n�A�/A�ƨA��^A��A�{A��A��yA��hA�33A��A��7A�XA�oA�9XA�x�A��
A���A���A���A�G�A��uA��A�I�A��!A�|�A��A���A��jA��FA��`A�ȴA���A���A��A�v�A��TA��uA�oA�`BA�{A��uA�JA��A���A��A�VA~��A}\)Az��Ax�+Awp�AuhsAst�Aq|�Ao/Am/Ajr�AgC�AeVAcXAa�PA_�wA^A[�AY�AX�AW&�AT��AS�
AS+AR�APM�AOdZAN�RAM�wAL=qAK%AJ{AHVAGG�AFAD�!AC��AB�ABr�AB�/AB�DAA�wA?�
A>v�A=dZA;��A:�/A:��A9��A9hsA8�jA7l�A6��A4�DA2��A1�wA0��A/��A.�\A-"�A+�
A+;dA*�A*jA)C�A(bNA'C�A%p�A$��A#�A#O�A"�+A"=qA!�A ��A7LA��A�+AM�A�At�A��AZAC�AZA��A�A��A��A�\A�A��AA/AȴAVA  AdZAE�A��AȴA��A�A
ȴA
A�A�yA�-AG�AffA�FA/A��A�A�+A��A|�AA ��A n�@��@��@���@�-@���@�33@�7L@�ȴ@�/@���@�$�@�G�@띲@�R@�^@�;d@��@��@�ƨ@�;d@���@�E�@�hs@ߍP@�v�@�X@�\)@��@�@�?}@���@�Ĝ@ץ�@��T@���@ӥ�@ёh@�z�@���@���@͙�@̃@�1'@��m@�|�@�S�@ʏ\@�=q@�{@�G�@ȣ�@ț�@ȓu@�(�@�S�@�5?@���@��@Å@��@��@���@��9@� �@�33@�~�@�@�%@� �@�K�@��-@��P@�;d@�$�@�&�@���@�r�@���@�t�@�33@�@��@���@�%@�I�@��;@�\)@�J@��@���@�t�@��R@��@���@��@�7L@��9@��u@�(�@���@�t�@���@�+@��R@�ff@�@��7@�V@��u@��w@�;d@��@���@��+@�=q@�@�@��h@�`B@��@��/@�bN@�  @���@��@�V@��@���@��7@�p�@�`B@�X@�`B@�/@��D@�1'@�  @��m@�b@�1@�+@��@���@���@���@�=q@��-@�O�@�/@�/@�/@�7L@�?}@�X@�V@��@�z�@�z�@�Q�@� �@��w@���@�33@��\@�ff@�5?@��h@�hs@�G�@��`@�r�@���@��F@���@�C�@�@�
=@�
=@�@���@�ȴ@���@��@���@�7L@��@��9@�bN@��@� �@�b@��;@�ƨ@��w@�t�@�|�@���@�dZ@�33@��!@�-@�$�@�E�@�{@���@���@���@���@���@��-@���@��@�x�@�hs@�G�@�/@�7L@���@��j@��j@��@��9@���@��@��@��m@���@��P@�l�@�;d@��@��@�o@�
=@��@���@�V@��@�@��7@�O�@�7L@�/@���@�r�@�I�@� �@�ƨ@�dZ@�"�@�"�@��@���@���@���@���@���@�V@�$�@���@�x�@�&�@��`@��9@�Z@�(�@�(�@���@�1@�b@��@�1@��m@��@���@���@���@�K�@���@�n�@�^5@���@�X@�X@�`B@�?}@�?}@�/@�%@��u@�9X@�1@�  @�@�P@\)@�P@�;@�  @� �@�;@\)@
=@~��@~ȴ@~E�@~{@}��@|��@|��@|�D@|z�@|j@|9X@|1@{�
@{�@{S�@z�@z��@y�#@y��@yX@y%@xĜ@x��@xQ�@xb@w�@w��@w�@wl�@w�@vv�@u�T@u�-@t��@tz�@t��@tj@s�
@s�F@s��@s�@s33@r=q@rJ@q�@q��@p��@p��@p  @p �@o�P@o\)@o;d@nȴ@n$�@m�@m��@m�@l�/@l��@lz�@l�@k�m@k�
@k�F@jM�@i�@i%@h�9@h �@g��@g;d@f��@fV@f{@e�@e@e�@d��@d�@d��@dZ@c�
@c��@co@b��@b�!@b�!@b~�@a��@`�`@_�;@_l�@_�@^��@^E�@^{@^@^@^@^@]�@]��@]`B@]V@\��@\j@[�m@[��@[dZ@[dZ@[C�@[o@Z��@Z��@Z^5@Z-@Y��@Y��@Y�7@YX@Y&�@Y%@X�9@X�u@Xr�@X �@W�P@V��@Vȴ@V��@V�+@VV@VE�@V{@U�T@U�-@U�@T��@T�D@T(�@S�@R�@RM�@Q��@QX@P��@P�u@O��@O��@O��@Ol�@O+@N�@Nȴ@N�R@N��@Nv�@NE�@N$�@Mp�@M?}@M�@MV@L�j@L�D@L9X@K��@K��@K�@KC�@J�@J�\@J=q@I��@I�7@I�@HbN@Hb@G�@G��@G��@Gl�@G\)@Fȴ@F�+@E@E�@DZ@C�
@Cƨ@C�@CC�@C"�@B�H@B~�@B=q@A��@A&�@A%@@��@@��@@�9@@r�@@1'@@  @?l�@?�@>��@>��@>v�@>V@>5?@=�@=��@=@=�h@=?}@=�@=�@=�@<�@<��@<z�@<1@;t�@;33@;"�@;"�@;o@:��@:M�@:-@:�@9�@9��@9x�@8��@81'@7�@7�w@7�@7�P@7|�@7|�@7K�@7
=@6�R@6V@6{@5@5p�@5`B@5?}@5�@4��@4�/@4��@4�D@4Z@41@3�m@3t�@3C�@333@3@2�H@2��@2�\@2^5@2=q@1��@1�#@1�^@1��@1x�@1X@1�@0�9@0�u@0�u@0r�@0b@/�P@/K�@.��@.E�@-��@-�-@-��@-`B@-?}@,�@,z�@,(�@+��@+C�@*�H@*��@*n�@*^5@*=q@*J@)��@)hs@(�`@(�@( �@(  @'��@'l�@'+@&�y@&�R@&��@&v�@&{@%�@%�@$��@$�@$j@$�@#��@#33@#@"n�@"J@!�^@!�7@!hs@!G�@!&�@!&�@!&�@!�@!%@ ��@ ��@  �@�w@�P@l�@l�@K�@;d@+@��@�@��@v�@v�@ff@$�@@��@p�@��@�j@�@z�@I�@Z@Z@9X@�@�m@ƨ@ƨ@��@�@t�@t�@dZ@dZ@S�@S�@C�@33@"�@�@�H@��@��@��@=q@��@�#@��@��@x�@X@7L@�@�@%@��@��@�9@�u@Q�@  @�@|�@l�@;d@�@�@v�@��@��@p�@?}@�/@��@�D@j@Z@Z@I�@9X@9X@(�@1@�@�@~�@M�@�@��@x�@X@7L@��@Ĝ@�@A�@b@�;@�w@�P@\)@;d@�@�@ff@V@V@E�@5?@{@�@@@@�-@�h@�@`B@O�@?}@?}@/@�@�@�/@�j@�D@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�NB�NB�NB�NB�HB�HB�NB�NB�TB�TB�fB�`B�`B�fB�mB�B�BDB+BK�B�VB��B�bB�uB�oB��B�hB��B�{B�{B�uB�hB�\B�PB�PB�PB�DB�DB�PB�bB�bB�\B�bB�bB��B��B��B��B��B��B��B�B�B��B��B��B�{B��B�{B�PB~�Bk�Bw�B�bB�uB�1B�B�DB�+By�B_;BH�BH�BI�BE�B5?B.B$�B{B�B��B�B� Bs�BdZB[#BYBYBXBQ�BI�B8RB%�B�B�BVB
��B
�B
�`B
�)B
�B
��B
ÖB
�-B
��B
��B
��B
�7B
|�B
q�B
e`B
ZB
G�B
49B
'�B
�B

=B	��B	�sB	��B	�qB	��B	�bB	�B	k�B	]/B	O�B	@�B	33B	)�B	�B	oB		7B	B��B��B�B�B�B�`B�/B�B�;B�5B�B��BɺBɺB��B�5B�BB�BB�
B��B��BB�wB��B�wB�qB�XB�FB�-B�B��B��B��B��B��B�uB�\B�JB�DB�DB�1B�B�B~�B|�Bz�Bx�Bu�Bt�Bq�Bo�BjBffBdZBcTBdZB]/B\)B[#BW
BW
BS�BS�BQ�BP�BM�BK�BJ�BG�BG�BF�BE�BD�BC�B>wB>wB<jB5?B33B49B2-B/B(�B&�B&�B%�B)�B2-B1'B/B-B-B,B+B)�B(�B%�B'�B'�B%�B'�B)�B$�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B)�B49B5?B49B49B33B49B5?B8RB;dB<jB<jB?}B?}BA�BB�BD�BE�BF�BH�BK�BM�BR�BS�BS�BT�BR�BVBW
BXBZB]/B`BBcTBl�Bn�Bn�Bn�Bm�Bn�Bp�Bp�Bp�Bo�Bm�Bk�Bm�Bn�Bo�Bp�Br�Bs�Bv�Bw�B{�B|�B� B�B�B�B�B�B�B�B�1B�JB�VB��B��B��B��B��B��B��B��B�B�'B�9B�FB�XB�^B�^B��BÖBǮB��B��B��B��B��B�
B�B�B�B�)B�5B�BB�TB�B�B��B��B��B��B��B��B��B��B��B��B	B	B	+B	JB	bB	�B	�B	�B	 �B	!�B	&�B	)�B	+B	.B	1'B	49B	6FB	7LB	6FB	;dB	=qB	>wB	?}B	B�B	C�B	E�B	H�B	K�B	K�B	O�B	P�B	R�B	VB	YB	[#B	\)B	^5B	`BB	cTB	dZB	gmB	hsB	jB	m�B	p�B	q�B	t�B	x�B	z�B	{�B	~�B	� B	� B	�B	�B	�%B	�JB	�VB	�bB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�'B	�'B	�-B	�-B	�?B	�FB	�XB	�dB	�qB	�wB	�wB	�}B	��B	B	B	ÖB	ĜB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�#B	�/B	�;B	�;B	�BB	�BB	�BB	�TB	�`B	�`B	�fB	�mB	�fB	�`B	�ZB	�ZB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
%B
%B
%B
+B
+B
+B
+B
+B
+B
+B
	7B
	7B
	7B
	7B
DB
JB
JB
JB
PB
VB
VB
VB
\B
\B
VB
VB
VB
PB
VB
\B
\B
\B
bB
\B
bB
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
hB
hB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
+B
+B
+B
,B
,B
,B
,B
-B
-B
-B
-B
-B
,B
-B
-B
-B
.B
.B
/B
/B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
7LB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
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
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
YB
ZB
ZB
ZB
ZB
ZB
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
\)B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
aHB
aHB
aHB
aHB
bNB
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
ffB
ffB
ffB
ffB
ffB
gmB
gmB
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
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�4B�4B�4B�-B�-B�B�4B�:B�:B�LB�FB�FB�LB�RB�kB�B)B*�BK�B�"B�yB�HB�[B�TB�YB�NB�YB�aB�FB�[B�4B�BB�6B�6B�6B�B�B�6B�HB�HB�BB�HB�HB�mB��B��B��B��B��B��B��B��B��B��B��B�FB��B�aB�B~�BkkBw�B�.B�@B�B��B�)B�By�B_!BH�BH�BI�BE�B5%B-�B$�BaB�|BοB��B�Bs�Bd&B[	BX�BX�BW�BQ�BI�B8B%�BgBMB"B
��B
�cB
�FB
��B
��B
ΥB
�aB
��B
��B
��B
�MB
�B
|�B
qvB
eFB
Y�B
G�B
4B
'�B
qB

	B	��B	�>B	��B	�VB	��B	�.B	��B	kkB	\�B	O�B	@iB	3B	)�B	�B	TB		B	�B��B��B�}B�]B�KB�,B��B�B�B�B��BΥBɠBɠB��B�B�B�B��BοBʌB�[B�BB�OB�]B�<B�>B�B�B��B��B��B��B�kB�YB�[B�(B�B�)B�)B��B��B��B~�B|�Bz�Bx�Bu�Bt�BqvBoiBjKBf2Bd&Bc Bd&B\�B[�BZ�BV�BV�BS�BS�BQ�BP�BM�BK�BJ�BG�BGzBFtBEmBD�BCaB>BB>BB<6B5B2�B4B1�B.�B(�B&�B&�B%�B)�B1�B0�B.�B,�B,�B+�B*�B)�B(�B%�B'�B'�B%�B'�B)�B$�B �B�BxBqB�BeB_BeB_BB_BYBYBYB_B�B�B�B!�B)�B4B5B4B4B2�B4B5B8B;0B<PB<6B?HB?HBAUBB[BDgBEmBFtBH�BK�BM�BR�BS�BS�BT�BR�BU�BV�BW�BY�B\�B`Bc BlWBncBncBncBm]BncBpoBpoBpoBoiBm]BkkBm]BncBoiBpUBr|Bs�Bv�Bw�B{�B|�B�B��B��B��B��B��B��B��B��B�B�"B�MB��B��B��B��B��B��B��B��B��B�B�B�$B�*B�*B�OB�aB�zB͟BΥBѷB��B��B��B��B��B��B��B�B�B� B�QB�vB�B��B��B��B��B��B��B��B��B��B	 �B	�B	�B	B	.B	SB	kB	~B	 �B	!�B	&�B	)�B	*�B	-�B	0�B	4B	6B	7B	6B	;JB	=VB	>(B	?.B	B[B	CaB	EmB	H�B	K�B	K�B	O�B	P�B	R�B	U�B	X�B	Z�B	[�B	^B	`B	c B	dB	g8B	h>B	jKB	mwB	poB	qvB	t�B	x�B	z�B	{�B	~�B	�B	�B	��B	��B	��B	�B	�"B	�.B	�@B	�FB	�FB	�FB	�FB	�MB	�YB	�_B	�_B	�kB	�qB	�xB	�~B	��B	��B	��B	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�$B	�0B	�<B	�BB	�BB	�HB	�UB	�[B	�[B	�aB	�gB	�tB	�zB	ȀB	ȚB	�lB	˒B	˒B	̘B	͟B	бB	ѷB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�B	�B	� B	�,B	�,B	�2B	�8B	�2B	�,B	�&B	�&B	�2B	�>B	�DB	�KB	�eB	�QB	�qB	�WB	�WB	�]B	�cB	�B	�B	�oB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
	B
	B
B
0B
0B
B
B
"B
"B
"B
(B
(B
"B
"B
"B
B
B
(B
(B
(B
.B
(B
HB
.B
HB
HB
.B
.B
.B
.B
.B
4B
4B
.B
.B
.B
.B
.B
.B
.B
.B
.B
B
B
.B
4B
4B
4B
4B
4B
4B
:B
@B
&B
@B
@B
FB
FB
MB
MB
MB
SB
SB
YB
YB
YB
_B
_B
eB
eB
eB
B
kB
kB
kB
qB
xB
~B
~B
~B
~B
~B
~B
~B
�B
�B
�B
�B
 �B
 vB
pB
�B
 �B
 �B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
+�B
,�B
,�B
,�B
-�B
-�B
.�B
.�B
/�B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
4B
4B
4B
4B
5B
5B
6B
6B
6B
6B
6B
7B
8B
9$B
:*B
:*B
:*B
:*B
:*B
;0B
;0B
;0B
;0B
;0B
;0B
;0B
;JB
;0B
;0B
;0B
;0B
<B
<6B
<6B
<6B
=<B
=<B
>BB
>BB
>BB
?cB
?HB
?HB
@OB
@OB
AUB
AUB
B[B
BuB
B[B
CaB
CaB
DgB
DgB
DgB
DMB
DgB
ESB
EmB
EmB
EmB
FtB
FtB
FtB
F�B
FtB
FtB
FtB
GzB
H�B
H�B
H�B
H�B
H�B
HfB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
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
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
[	B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\B
\B
[�B
\B
\�B
\�B
\�B
^B
]�B
^B
^B
_B
_!B
_B
`B
aB
aB
aB
a-B
b4B
bB
bB
c B
c B
c B
c B
c B
c B
c:B
c B
c:B
d&B
d&B
d&B
d&B
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
gRB
g8B
g8B
hXB
h>B
h>B
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
iDB
iDB
i_B
i_B
iDB
iDB
iDB
i*B
jKB
jeB
jK11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.4(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201807090049072018070900490720180709004907201807100034222018071000342220180710003422JA  ARFMdecpA19c                                                                20180723153756  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180723070918  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180723070923  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180723070923  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180723070925  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180723070925  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180723070925  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180723070925  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20180723070926                      G�O�G�O�G�O�                JA  ARUP                                                                        20180723072027                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180704154222  CV  JULD            G�O�G�O�F�y�                JM  ARCAJMQC2.0                                                                 20180708154907  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180708154907  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180709153422  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                