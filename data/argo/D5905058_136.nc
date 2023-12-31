CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-04-03T15:36:59Z creation;2019-04-03T15:37:02Z conversion to V3.1;2019-12-23T06:04:22Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20190403153659  20200120031518  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_136                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @س�Z�% 1   @س��l @8Ů�1���c5XbM�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A���A���A���A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Ca�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�
=A�
=A�
=A�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�B�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��CbnCd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~�HC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Du!�Du��Dv!�Dv��Dw!�Dw��Dx!�Dx��Dy!�Dy��Dz!�Dz��D{!�D{��D|!�D|��D}!�D}��D~!�D~��D!�D��D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�T)D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D�D���D��D�P�DÐ�D���D��D�P�DĐ�D���D��D�P�DŐ�D���D��D�P�DƐ�D���D��D�P�Dǐ�D���D��D�P�DȐ�D���D��D�P�Dɐ�D���D��D�P�Dʐ�D���D��D�P�Dː�D���D��D�P�D̐�D���D��D�P�D͐�D���D��D�P�Dΐ�D���D��D�P�Dϐ�D���D��D�P�DА�D���D��D�P�Dѐ�D���D��D�P�DҐ�D���D��D�P�DӐ�D���D��D�P�DԐ�D���D��D�P�DՐ�D���D��D�P�D֐�D���D��D�P�Dא�D���D��D�P�Dؐ�D���D��D�P�Dِ�D���D��D�P�Dڐ�D���D��D�P�Dې�D���D��D�P�Dܐ�D���D��D�P�Dݐ�D���D��D�P�Dސ�D���D��D�P�Dߐ�D���D��D�P�D���D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D��)D�)D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��TA��`A��mA��`A��TA��mA��yA��mA��yA��yA��;A��/A��/A���A��FA���A�n�A�O�A��A�A���A���A��TA��A���A�ȴA��FA��!A���A��7A�jA�Q�A�K�A�JA���A��#A���A�~�A��A�dZA�XA�1A� �A�ƨA��A�|�A���A�ZA���A�t�A�{A��yA�XA�S�A�"�A�z�A���A�(�A�ffA���A�ZA���A���A�M�A�JA�\)A��7A�p�A��A�-A���A�VA�ȴA�r�A�(�A�ƨA�r�A��RA{�hAyoAw/Au�Ast�Ar�+Aq�PAp�/ApbNAl��AiO�Ah�HAh��Ah=qAg|�Af9XAd�Abr�A`�A_�A]��A[;dAZ�AX�jAT��AO33AK��AJE�AH�/AG�AES�AD�HACl�AAp�AAoA>��A=hsA;��A:�A9�
A9+A7��A7�A6bNA4�!A3l�A3%A1�FA0Q�A0bA/%A.9XA-�wA,��A+/A*��A*bNA*Q�A)�A(�HA'��A'`BA&��A%�;A$�A#��A#oA ��A�uAbA�A�AM�A�AJA�7A�jA�;A��AAƨAXA��Az�AbNAE�A(�A�A��A/A��AVA�`A�AS�AjA�A�#A|�A
�/A
I�A	l�A^5A�`A��A�hAȴA��A^5AƨA -@���@�Ĝ@���@�l�@��R@��@��@�9X@��
@��#@���@�ff@���@�V@�P@ꟾ@�@��m@�^@��@���@�S�@�J@��@ߍP@��H@�~�@�ff@�v�@�v�@�ff@�V@�E�@�J@ݡ�@�O�@ۥ�@١�@؛�@�K�@�bN@�
=@�{@�hs@�S�@Ώ\@�@̴9@�I�@� �@�C�@�hs@�9X@�^5@�O�@��@���@��@�/@���@�1@���@�l�@���@�v�@�V@�-@��@�X@�b@��@���@��j@�9X@��
@�l�@�ff@�/@��;@��P@�"�@�~�@��^@��@��j@��m@�S�@�o@��y@��H@��H@��H@��H@��@���@�^5@���@�Ĝ@�Q�@�1'@��;@�S�@�-@��-@�@�O�@���@���@�t�@�dZ@�l�@�l�@�K�@�33@�ȴ@��\@�v�@�{@���@�@��@���@�&�@��@�(�@�l�@�5?@��T@��h@��`@��9@��u@�r�@�Q�@�1@�  @��@�S�@�t�@�|�@�33@�@���@�E�@�@��@��D@��m@��P@�|�@�t�@��H@�n�@���@��#@��-@�`B@�7L@�/@��`@���@�Q�@�(�@�b@���@��m@��
@��F@��P@�C�@�ȴ@���@�-@��#@���@�x�@�x�@�p�@�p�@�`B@�/@�V@��@��@���@�l�@�"�@���@��@��H@��!@�E�@��#@���@��h@�x�@�`B@�X@�7L@�%@��@��/@���@��9@��D@�z�@�I�@��m@���@��w@��P@�;d@�o@���@��H@�ȴ@���@�V@��@�J@���@��T@�@���@�hs@�`B@�X@�G�@�7L@��`@��@�z�@�j@�A�@���@��@�l�@�S�@�C�@�;d@�"�@�
=@�@��H@��@���@�ȴ@��!@�v�@�^5@�E�@�=q@�$�@�J@��@��T@���@���@��@�O�@�&�@��`@���@�Z@�I�@�Q�@�I�@� �@���@�ƨ@���@�t�@�C�@�
=@��!@�~�@�v�@�n�@�ff@�5?@�@���@��^@���@���@��7@��@�x�@�p�@�X@�?}@��/@�z�@�j@� �@��m@�ƨ@��P@�@���@�M�@�=q@�$�@�@���@���@�p�@�7L@��/@�j@�A�@��@�1@�  @�  @�  @�  @�;@��@�@~�y@~��@~v�@~5?@}�@}p�@}/@|��@|�j@|I�@{33@z�H@z��@z��@z��@z�\@zn�@y��@yG�@x��@xb@w�;@v��@vff@vV@v@up�@u/@uV@t��@t��@t��@t�D@tj@t1@st�@sC�@so@r��@r��@r~�@rM�@r-@q�7@q%@pĜ@o�@o|�@oK�@n�y@n�R@n�+@nff@nff@n$�@m�@mO�@m?}@m/@mV@lj@l(�@l1@kƨ@k�F@k��@k�@kt�@kdZ@k"�@j��@j��@j~�@j~�@j~�@jn�@j=q@jJ@i��@i��@i�@hr�@hb@g�;@g�w@gl�@f��@f�@f5?@e�h@eO�@d�@dZ@d�@cƨ@c��@c�@cdZ@cS�@b�@b^5@a��@`�`@`Ĝ@`r�@`b@_�@_\)@^�y@^�+@]@]�@]�@\�@\��@\�@\�@\�D@\j@\I�@\(�@\�@[�m@[ƨ@[ƨ@[ƨ@[��@[33@[@Z�\@Z-@ZJ@Y��@Y�@Y�#@Yx�@Y&�@X��@X�`@X��@XQ�@X �@X  @W�;@W��@W|�@W+@V�y@V��@Vv�@V$�@U�T@U�h@U?}@T�j@T�D@T�@S�
@S��@St�@S@Rn�@RM�@R-@RJ@Q�#@Qhs@Q&�@P��@P��@P�@PbN@P1'@P1'@P �@O�w@O�@N�+@N@M�T@M�@L�/@Lz�@L9X@L�@L1@Kƨ@K��@K"�@J��@J�@I��@Ihs@I7L@H�u@HQ�@H �@H  @G�@G�@G+@Fȴ@FV@E��@E�h@Ep�@E�@D�@D�/@D�D@C�
@CC�@Co@C@B��@B=q@A�#@Ahs@A&�@@��@@��@@ �@?�;@?�P@?�@>v�@>@=`B@=?}@<��@<�/@<�j@<�@<��@<(�@;�m@;�F@;S�@;"�@:�@:��@:n�@:J@9�7@9hs@9&�@8�`@8��@8��@8A�@7��@7�@7|�@7�@6{@5�h@5�@5/@4z�@3�F@3t�@3"�@3@2�@2��@2~�@2=q@1�#@1�#@1X@1x�@1�@0��@0r�@0r�@0r�@0Q�@0 �@0b@/�P@/;d@/�@.�@.ȴ@.��@.{@-��@-�-@-@-�@-O�@,��@,��@,��@,�D@,9X@+��@+�@+t�@+dZ@+"�@*�H@*��@*�\@*n�@*M�@*-@)��@)��@)�^@)�^@)��@)�7@)X@)7L@)�@)%@)%@(��@(�9@(A�@(  @'�P@'K�@';d@';d@'+@'�@&�@&$�@&@&@%�@%�h@%/@$�@$Z@$9X@$�@$1@#�F@#��@#t�@#@"�!@"n�@"�@!�@!��@!�^@!��@!X@!�@!%@ ��@ ��@ bN@ Q�@ 1'@�@��@��@K�@
=@�y@�@ȴ@��@�-@p�@`B@/@��@z�@Z@Z@I�@I�@I�@I�@�m@�@C�@"�@��@^5@��@�#@��@7L@��@A�@�@��@;d@��@��@ff@$�@@��@?}@/@V@��@��@z�@z�@j@Z@Z@(�@��@�F@t�@S�@"�@�!@~�@n�@^5@M�@�@J@��@�#@��@x�@X@�@Ĝ@�@b@|�@|�@l�@l�@\)@\)@K�@;d@��@E�@{@�T@@��@O�@��@��@��@(�@��@�
@ƨ@�F@�F@��@��@��@�@t�@o@
�@
�H@
�!@
n�@
M�@
�@	�#@	��@	��@	�7@	x�@	hs@	X@��@��@Ĝ@�u@Q�@  @��@�@l�@\)@K�@+@��@ȴ@��@v�@ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��TA��`A��mA��`A��TA��mA��yA��mA��yA��yA��;A��/A��/A���A��FA���A�n�A�O�A��A�A���A���A��TA��A���A�ȴA��FA��!A���A��7A�jA�Q�A�K�A�JA���A��#A���A�~�A��A�dZA�XA�1A� �A�ƨA��A�|�A���A�ZA���A�t�A�{A��yA�XA�S�A�"�A�z�A���A�(�A�ffA���A�ZA���A���A�M�A�JA�\)A��7A�p�A��A�-A���A�VA�ȴA�r�A�(�A�ƨA�r�A��RA{�hAyoAw/Au�Ast�Ar�+Aq�PAp�/ApbNAl��AiO�Ah�HAh��Ah=qAg|�Af9XAd�Abr�A`�A_�A]��A[;dAZ�AX�jAT��AO33AK��AJE�AH�/AG�AES�AD�HACl�AAp�AAoA>��A=hsA;��A:�A9�
A9+A7��A7�A6bNA4�!A3l�A3%A1�FA0Q�A0bA/%A.9XA-�wA,��A+/A*��A*bNA*Q�A)�A(�HA'��A'`BA&��A%�;A$�A#��A#oA ��A�uAbA�A�AM�A�AJA�7A�jA�;A��AAƨAXA��Az�AbNAE�A(�A�A��A/A��AVA�`A�AS�AjA�A�#A|�A
�/A
I�A	l�A^5A�`A��A�hAȴA��A^5AƨA -@���@�Ĝ@���@�l�@��R@��@��@�9X@��
@��#@���@�ff@���@�V@�P@ꟾ@�@��m@�^@��@���@�S�@�J@��@ߍP@��H@�~�@�ff@�v�@�v�@�ff@�V@�E�@�J@ݡ�@�O�@ۥ�@١�@؛�@�K�@�bN@�
=@�{@�hs@�S�@Ώ\@�@̴9@�I�@� �@�C�@�hs@�9X@�^5@�O�@��@���@��@�/@���@�1@���@�l�@���@�v�@�V@�-@��@�X@�b@��@���@��j@�9X@��
@�l�@�ff@�/@��;@��P@�"�@�~�@��^@��@��j@��m@�S�@�o@��y@��H@��H@��H@��H@��@���@�^5@���@�Ĝ@�Q�@�1'@��;@�S�@�-@��-@�@�O�@���@���@�t�@�dZ@�l�@�l�@�K�@�33@�ȴ@��\@�v�@�{@���@�@��@���@�&�@��@�(�@�l�@�5?@��T@��h@��`@��9@��u@�r�@�Q�@�1@�  @��@�S�@�t�@�|�@�33@�@���@�E�@�@��@��D@��m@��P@�|�@�t�@��H@�n�@���@��#@��-@�`B@�7L@�/@��`@���@�Q�@�(�@�b@���@��m@��
@��F@��P@�C�@�ȴ@���@�-@��#@���@�x�@�x�@�p�@�p�@�`B@�/@�V@��@��@���@�l�@�"�@���@��@��H@��!@�E�@��#@���@��h@�x�@�`B@�X@�7L@�%@��@��/@���@��9@��D@�z�@�I�@��m@���@��w@��P@�;d@�o@���@��H@�ȴ@���@�V@��@�J@���@��T@�@���@�hs@�`B@�X@�G�@�7L@��`@��@�z�@�j@�A�@���@��@�l�@�S�@�C�@�;d@�"�@�
=@�@��H@��@���@�ȴ@��!@�v�@�^5@�E�@�=q@�$�@�J@��@��T@���@���@��@�O�@�&�@��`@���@�Z@�I�@�Q�@�I�@� �@���@�ƨ@���@�t�@�C�@�
=@��!@�~�@�v�@�n�@�ff@�5?@�@���@��^@���@���@��7@��@�x�@�p�@�X@�?}@��/@�z�@�j@� �@��m@�ƨ@��P@�@���@�M�@�=q@�$�@�@���@���@�p�@�7L@��/@�j@�A�@��@�1@�  @�  @�  @�  @�;@��@�@~�y@~��@~v�@~5?@}�@}p�@}/@|��@|�j@|I�@{33@z�H@z��@z��@z��@z�\@zn�@y��@yG�@x��@xb@w�;@v��@vff@vV@v@up�@u/@uV@t��@t��@t��@t�D@tj@t1@st�@sC�@so@r��@r��@r~�@rM�@r-@q�7@q%@pĜ@o�@o|�@oK�@n�y@n�R@n�+@nff@nff@n$�@m�@mO�@m?}@m/@mV@lj@l(�@l1@kƨ@k�F@k��@k�@kt�@kdZ@k"�@j��@j��@j~�@j~�@j~�@jn�@j=q@jJ@i��@i��@i�@hr�@hb@g�;@g�w@gl�@f��@f�@f5?@e�h@eO�@d�@dZ@d�@cƨ@c��@c�@cdZ@cS�@b�@b^5@a��@`�`@`Ĝ@`r�@`b@_�@_\)@^�y@^�+@]@]�@]�@\�@\��@\�@\�@\�D@\j@\I�@\(�@\�@[�m@[ƨ@[ƨ@[ƨ@[��@[33@[@Z�\@Z-@ZJ@Y��@Y�@Y�#@Yx�@Y&�@X��@X�`@X��@XQ�@X �@X  @W�;@W��@W|�@W+@V�y@V��@Vv�@V$�@U�T@U�h@U?}@T�j@T�D@T�@S�
@S��@St�@S@Rn�@RM�@R-@RJ@Q�#@Qhs@Q&�@P��@P��@P�@PbN@P1'@P1'@P �@O�w@O�@N�+@N@M�T@M�@L�/@Lz�@L9X@L�@L1@Kƨ@K��@K"�@J��@J�@I��@Ihs@I7L@H�u@HQ�@H �@H  @G�@G�@G+@Fȴ@FV@E��@E�h@Ep�@E�@D�@D�/@D�D@C�
@CC�@Co@C@B��@B=q@A�#@Ahs@A&�@@��@@��@@ �@?�;@?�P@?�@>v�@>@=`B@=?}@<��@<�/@<�j@<�@<��@<(�@;�m@;�F@;S�@;"�@:�@:��@:n�@:J@9�7@9hs@9&�@8�`@8��@8��@8A�@7��@7�@7|�@7�@6{@5�h@5�@5/@4z�@3�F@3t�@3"�@3@2�@2��@2~�@2=q@1�#@1�#@1X@1x�@1�@0��@0r�@0r�@0r�@0Q�@0 �@0b@/�P@/;d@/�@.�@.ȴ@.��@.{@-��@-�-@-@-�@-O�@,��@,��@,��@,�D@,9X@+��@+�@+t�@+dZ@+"�@*�H@*��@*�\@*n�@*M�@*-@)��@)��@)�^@)�^@)��@)�7@)X@)7L@)�@)%@)%@(��@(�9@(A�@(  @'�P@'K�@';d@';d@'+@'�@&�@&$�@&@&@%�@%�h@%/@$�@$Z@$9X@$�@$1@#�F@#��@#t�@#@"�!@"n�@"�@!�@!��@!�^@!��@!X@!�@!%@ ��@ ��@ bN@ Q�@ 1'@�@��@��@K�@
=@�y@�@ȴ@��@�-@p�@`B@/@��@z�@Z@Z@I�@I�@I�@I�@�m@�@C�@"�@��@^5@��@�#@��@7L@��@A�@�@��@;d@��@��@ff@$�@@��@?}@/@V@��@��@z�@z�@j@Z@Z@(�@��@�F@t�@S�@"�@�!@~�@n�@^5@M�@�@J@��@�#@��@x�@X@�@Ĝ@�@b@|�@|�@l�@l�@\)@\)@K�@;d@��@E�@{@�T@@��@O�@��@��@��@(�@��@�
@ƨ@�F@�F@��@��@��@�@t�@o@
�@
�H@
�!@
n�@
M�@
�@	�#@	��@	��@	�7@	x�@	hs@	X@��@��@Ĝ@�u@Q�@  @��@�@l�@\)@K�@+@��@ȴ@��@v�@ff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�DB
�JB
�JB
�JB
�JB
�JB
�JB
�JB
�DB
�DB
�DB
�DB
�DB
�=B
�=B
�1B
�+B
�%B
�B
�%B
�+B
�7B
�=B
�DB
�DB
�oB
��B
��B
�XB
��B
�yB
��BB�B5?Be`B�JB�uB��B��B��B�DB� Be`B[#BXBN�BA�B1'B&�B$�B(�B&�B�BL�Be`BgmBaHBXBJ�B1'BVB
��B
�B
�B
��B
�hB
�B
k�B
^5B
?}B
9XB
@�B
G�B
)�B	��B	�`B	ĜB	��B	�JB	�B	�DB	�1B	� B	x�B	r�B	k�B	T�B	8RB	1'B	/B	,B	'�B	 �B	�B	
=B��B��B��B��B�B�B�NBɺB�B�B��B��B�{B�oB�hB�DB�DB�PB�%B�B�B}�B~�By�Bs�Bo�BjBgmBe`BffBbNBaHBaHB_;BaHBcTBe`Be`BdZBcTBbNB_;B[#BZBZBXBVBVBVBW
BO�BM�BL�BG�BC�BD�BB�BB�BA�B@�B>wB=qB;dB:^B8RB8RB7LB7LB7LB8RB>wBF�BI�BP�BVBVBR�BK�BI�BG�BD�BA�B=qB9XB6FB49B2-B8RB>wBB�BA�BF�BK�BR�BW
B\)BbNBe`BffBe`BcTBaHB`BBZBXBR�BN�BG�BF�BD�BB�B@�B?}B>wB=qB<jB;dB:^B:^B<jB?}BB�BD�BD�BD�BE�BE�BD�BD�BD�BE�BF�BL�BO�BK�BL�BJ�BI�BI�BJ�BK�BK�BJ�BJ�BK�BM�BO�BP�BP�BR�BT�BVBW
BXBXBXBYBYBZBZBZB[#B\)B]/B`BBbNBcTBdZBffBjBo�Bt�Bu�Bw�Bz�B}�B�B�B�%B�1B�=B�DB�JB�VB�VB�\B�\B�hB�oB�{B��B��B��B��B��B��B�B�B�9B�FB�RB�jBBĜBɺB��B��B��B��B��B��B��B��B�
B�B�)B�BB�HB�`B�B�B�B��B��B��B��B��B��B	  B	B		7B	VB	{B	{B	�B	�B	�B	�B	�B	#�B	#�B	"�B	$�B	'�B	+B	-B	1'B	2-B	49B	7LB	7LB	8RB	:^B	;dB	<jB	<jB	=qB	>wB	?}B	@�B	A�B	C�B	E�B	J�B	K�B	P�B	R�B	T�B	VB	W
B	W
B	W
B	XB	ZB	[#B	`BB	cTB	e`B	gmB	hsB	iyB	iyB	iyB	k�B	n�B	q�B	s�B	s�B	t�B	u�B	u�B	v�B	x�B	y�B	z�B	z�B	{�B	}�B	}�B	� B	�B	�B	�B	�+B	�7B	�DB	�JB	�JB	�PB	�\B	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�?B	�LB	�RB	�RB	�XB	�^B	�qB	�wB	�}B	��B	B	B	ÖB	ÖB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�BB	�TB	�ZB	�ZB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B
DB
DB
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
VB
\B
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
oB
hB
hB
hB
oB
oB
oB
oB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
 �B
 �B
 �B
!�B
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
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
+B
,B
,B
-B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
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
49B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
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
M�B
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
N�B
N�B
O�B
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
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
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
YB
ZB
ZB
ZB
[#B
[#B
[#B
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
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
�B
�B
�0B
�B
�B
�B
�B
�0B
�B
�B
�)B
�B
�B
�	B
�	B
��B
��B
��B
��B
��B
��B
�B
�	B
�B
�B
�:B
��B
��B
�>B
ҽB
�DB
��B�B�B5Be,B�B�@B�YB��B��B�B�Be,BZ�BW�BN�BAUB0�B&�B$�B(�B&�B�BL�Be,Bg8BaBW�BJ�B0�B"B
��B
�B
��B
�kB
�4B
��B
kQB
^B
?HB
9$B
@OB
GzB
)�B	��B	�,B	�gB	��B	�B	��B	��B	��B	�B	x�B	r|B	kQB	T�B	8B	0�B	.�B	+�B	'�B	 �B	SB	
	B��B��B��B��B�B�iB��BɆB��B��B��B��B�FB�:B�4B�B��B�B��B��B��B}�B~�By�BshBoiBj0BgBeBf2BbB`�BaB_BaBcBe,BeBd&Bc BbB^�BZ�BY�BY�BW�BU�BU�BU�BV�BO�BM�BL�BGzBCaBDgBB[BB[BAUB@4B>BB=<B;0B:B8B8B6�B7B7B8B>(BFtBI�BP�BU�BU�BR�BK�BIlBGzBDgBA;B=<B9	B6B3�B1�B8B>(BB[BA;BFtBK�BR�BV�B[�BbBe,Bf2BeBcB`�B_�BY�BW�BR�BN�BG_BFtBDgBB[B@4B?.B>(B=<B<6B;0B:B:*B<6B?HBBABDMBDgBDMBESBESBDgBDMBDMBESBFYBL~BO�BK�BL~BJrBIlBI�BJrBKxBKxBJrBJrBK�BM�BO�BP�BP�BR�BT�BU�BV�BW�BW�BW�BX�BX�BY�BY�BY�BZ�B[�B\�B`Ba�BcBd&BfBj0BoiBtnBu�Bw�Bz�B}�B��B��B��B��B�	B�B��B�"B�B�B�(B�4B� B�,B�QB�xB�~B�jB��B��B��B��B�B��B�B�B�[B�gB�lB˒B˒BΊBЗBѝBҽBҽB��BּB��B��B��B�B�,B�WB�iB�[B��B��B��B��B��B��B��B	�B		B	B	,B	FB	2B	_B	xB	�B	�B	#�B	#�B	"�B	$�B	'�B	*�B	,�B	0�B	1�B	3�B	6�B	7B	8B	:B	;0B	<B	<B	="B	>BB	?.B	@4B	AUB	CGB	ESB	JrB	KxB	P�B	R�B	T�B	U�B	V�B	V�B	V�B	W�B	Y�B	Z�B	`B	c B	e,B	gB	h>B	iDB	i*B	i*B	k6B	nIB	q[B	shB	shB	t�B	utB	utB	v�B	x�B	y�B	z�B	z�B	{�B	}�B	}�B	�B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�(B	�B	�&B	�FB	�FB	�SB	�YB	�kB	�]B	�~B	�jB	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�	B	�B	�<B	�(B	�.B	�UB	�AB	�AB	�GB	�aB	�MB	�SB	�YB	�_B	ȀB	�lB	�rB	�rB	�~B	̈́B	͟B	ΥB	ϑB	ЗB	ѝB	ңB	өB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	��B	�B	�B	� B	�B	�B	�B	�B	�B	�>B	�6B	�QB	�=B	�WB	�CB	�]B	�IB	�cB	�OB	�iB	�UB	�aB	�aB	�hB	�hB	�B	�hB	�nB	�B	�B	�B	�tB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B

�B

�B
�B
�B
B
B
B
B
B
B
"B
B
B
B
.B
B
.B
B
B
.B
B
B
4B
B
B
 B
B
4B
B
:B
:B
 B
:B
&B
,B
,B
,B
,B
2B
MB
MB
9B
SB
?B
YB
EB
EB
_B
_B
EB
EB
EB
eB
KB
QB
kB
QB
WB
qB
WB
qB
xB
]B
~B
~B
�B
�B
�B
jB
�B
�B
jB
jB
jB
pB
pB
�B
pB
�B
pB
 �B
 �B
 vB
!|B
!|B
!�B
!|B
!|B
!�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
,�B
,�B
,�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
/�B
/�B
0�B
0�B
0�B
1�B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3�B
3�B
5B
5B
5B
4�B
4�B
5B
4�B
5�B
7B
7B
6�B
8B
8B
8B
8B
8B
8B
8B
9$B
9	B
9	B
:*B
:B
;B
;B
;0B
<B
<B
<B
<B
<B
<B
="B
="B
="B
="B
>(B
>BB
>BB
>BB
?.B
>BB
?.B
?.B
?.B
?HB
?HB
?.B
?.B
?.B
@4B
@4B
@OB
@4B
@OB
AUB
BAB
BAB
BAB
CGB
CaB
CGB
CaB
CGB
CaB
CGB
DgB
DMB
ESB
DMB
ESB
EmB
EmB
EmB
EmB
FYB
FtB
FYB
FtB
G_B
GzB
GzB
G_B
G_B
G_B
HfB
H�B
H�B
HfB
I�B
IlB
I�B
IlB
I�B
I�B
IlB
I�B
JrB
JrB
J�B
KxB
KxB
K�B
KxB
L�B
L�B
L~B
L~B
L~B
L�B
M�B
M�B
M�B
M�B
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
N�B
N�B
O�B
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
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
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
V�B
V�B
V�B
V�B
V�B
V�B
W�B
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
X�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
]�B
]�B
^B
^�B
_B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`B
_�B
_�B
_�B
_�B
`�B
aB
`�B
`�B
bB
a�B
bB
a�B
bB
bB
a�B
a�B
a�B
cB
cB
dB
dB
d&B
eB
fB
fB
fB
f2B
f2B
fB
fB
fB
f2B
g8B
gB
h>B
h>B
h$B
h>B
h$B
i*B
h>B
i*B
i*B
j0B
jKB
j0B
jKB
jKB
j0B
j0B
j0B
jKB
j0B
k6B
kQB
k6B
k6B
k6B
kQB
kQB
lWB
l=B
l=B
lWB
l=B
l=B
lWB
mCB
m]B
m]B
m]B
m]B
ncB
nIB
nIB
nIB
ncB
oOB
oiB
oiB
oiB
oiB
pUB
pU111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.53(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904090035022019040900350220190409003502201904100027122019041000271220190410002712JA  ARFMdecpA19c                                                                20190404003639  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190403153659  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190403153701  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190403153701  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190403153702  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190403153702  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190403153702  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190403153702  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190403153702  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190403153702                      G�O�G�O�G�O�                JA  ARUP                                                                        20190403155508                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190403153513  CV  JULD            G�O�G�O�FŜ�                JM  ARCAJMQC2.0                                                                 20190408153502  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190408153502  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190409152712  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031518                      G�O�G�O�G�O�                