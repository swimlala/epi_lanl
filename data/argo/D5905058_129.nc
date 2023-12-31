CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-03-04T15:37:31Z creation;2019-03-04T15:37:34Z conversion to V3.1;2019-12-23T06:06:00Z update;     
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20190304153731  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_129                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ج�o��1   @ج��7 @8Fs�����c464�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D���D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�C3D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�vf111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @N�R@���@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Du!�Du��Dv!�Dv��Dw!�Dw��Dx!�Dx��Dy!�Dy��Dz!�Dz��D{!�D{��D|!�D|��D}!�D}��D~!�D~��D!�D��D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�T)D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D�D���D��D�P�DÐ�D���D��D�P�DĐ�D���D��D�P�DŐ�D���D��D�P�DƐ�D���D��D�P�Dǐ�D���D��D�P�DȐ�D���D��D�P�Dɐ�D���D��D�P�Dʐ�D���D��D�P�Dː�D���D��D�P�D̐�D���D��D�P�D͐�D���D��D�P�Dΐ�D���D��D�P�Dϐ�D���D��D�P�DА�D���D��D�P�Dѐ�D���D��D�P�DҐ�D���D��D�P�DӐ�D���D��D�P�DԐ�D���D��D�P�DՐ�D���D��D�P�D֐�D���D��D�P�Dא�D���D��D�P�Dؐ�D���D��D�P�Dِ�D���D��D�P�Dڐ�D���D��D�P�Dې�D���D��D�P�Dܐ�D���D��D�P�Dݐ�D���D��D�P�Dސ�D���D��D�P�Dߐ�D���D��D�T)D���D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�T)D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�T)D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��/A��A���A���A�ȴA���A��A��TA��mA��TA��A��A��A��A��mA��A��A��A��A��A���A���A���A���A��DA��`A��A�Q�A�9XA�|�A��7A�|�A�ffA�E�A�=qA�7LA�dZA�VA��yA���A��yA��yA��;A��wA��
A��A��A��A���A�x�A���A�A�XA��#A�bNA���A�ZA�dZA��A�z�A���A�bNA�"�A��7A�7LA���A���A��A��A���A��A��uA��A�9XA�M�A�A�C�A�=qA���A�1A�O�A�5?A�;dA�bA���A�x�A�VA��A�{A�33A�ZA���A�r�A�A��`A���A��A�M�A��A��#A���A���A��\A�
=A��A��-A���A��mA��A���A���A��
A���A�VA���A��\A}p�Ayl�Av�yAs�
Aq\)Anv�AlȴAk33Ah�RAgl�Ad5?AbVA_G�A^�\A]AZ�HAY�PAWO�AU�mASƨAQhsAPz�AO��AO/ANjAL~�AJAH�`AHE�AGl�AFZAD��ACC�AB�`AB�A@�DA?�wA>�9A=A;�wA:��A:�DA8ĜA7�mA7VA65?A5|�A3��A2I�A1;dA.�A-�
A,^5A,$�A,�DA+��A*n�A)�A(r�A'��A&ZA$bNA"1'A �!AƨA�A7LA�;A|�A��A�A�-A��A��A33A��A�mA|�A�A�7A=qA��A%A�9AffA�#A�7A%A�#A�\A
�jA	dZAA�A�A{A&�Ar�A1A�-A�RA��A
=A bN@�l�@���@��9@�t�@��@��w@���@���@�Q�@�;d@�7L@�"�@�7L@�@ꗍ@�7L@旍@��;@�+@�v�@�&�@��@�S�@�-@݁@܋D@ۮ@ڇ+@�=q@ٲ-@�7L@�9X@�\)@���@�/@�C�@ѩ�@���@�9X@ϥ�@���@���@�Ĝ@�M�@�hs@ȓu@�7L@ɡ�@��@��T@�`B@���@�9X@�o@�n�@�=q@š�@���@§�@�V@���@��F@�ȴ@�v�@�E�@��@��7@�&�@��`@��@�dZ@�V@�%@�9X@�ƨ@���@�
=@��@��@��@�Ĝ@�  @�C�@��!@�n�@��#@��@�1'@�l�@���@�x�@��@���@�C�@�@��R@�-@���@�G�@��`@�(�@��w@��@�C�@��R@�{@��7@�/@�z�@��
@�\)@�
=@���@�n�@�@���@�x�@��@��@���@��j@�r�@�1'@�1@��m@�ƨ@�t�@�dZ@�K�@�K�@�K�@�;d@�o@��@���@���@��+@�5?@��^@�O�@��u@�Z@�(�@��m@��@���@��@���@�x�@�p�@�%@��j@��@�z�@��@�ƨ@�l�@�o@���@���@��+@�~�@�ff@�$�@��@���@��-@���@��@�p�@�`B@�7L@��9@���@�z�@�(�@��
@��@�K�@�33@��H@��!@��\@���@���@��+@�v�@�V@�5?@���@��T@��^@�O�@�/@�&�@���@��9@��j@��9@�r�@� �@�I�@�A�@��m@��P@�S�@���@��y@���@���@��!@���@�E�@�J@�@���@�x�@�?}@��/@���@��@��@�%@�  @���@��P@�dZ@�S�@�33@��@��y@��H@���@��!@���@�^5@�M�@�=q@�J@��@���@�@��^@���@���@��h@�X@��@�Ĝ@��D@�z�@�r�@�9X@�(�@��m@�t�@�;d@��@��R@���@��+@�V@�J@��@���@���@�p�@�O�@�7L@���@��@��`@���@��@��D@�z�@�bN@� �@��;@���@��@��P@�|�@�S�@�33@��@���@�ff@�=q@�{@���@��@���@���@��@�/@���@�bN@� �@�b@� �@�w@+@~��@}�T@}`B@}�@|j@|(�@|1@{ƨ@{��@{S�@z��@z^5@z=q@zJ@zJ@y�@y�7@xĜ@xr�@w�;@w�@v�+@v5?@v{@u��@u/@t�j@s�m@s33@r�@r��@r^5@r=q@r�@q�7@q��@qhs@pA�@o�w@ol�@n�@n$�@n@m@m�-@m�h@mO�@l��@l��@lZ@kƨ@kC�@k"�@k@j�H@j��@j~�@j�@i��@i�^@ix�@iG�@i&�@i�@h�u@hA�@g|�@g�@f�@fv�@fE�@fE�@fE�@f{@e�-@e?}@d��@d�@d��@d�j@d��@d9X@c�
@ct�@c"�@co@b��@b~�@bM�@a�^@ahs@a&�@`Ĝ@`r�@`b@_�w@_
=@^��@^V@^$�@]��@]p�@\��@\��@\��@[�m@[dZ@[33@Z��@Z��@Zn�@Y��@Y�7@YG�@X��@Xb@W�@W��@W|�@Wl�@W;d@V�y@V��@VE�@U��@U?}@T�@T�D@TI�@T�@St�@SdZ@R�@R~�@RM�@Q��@Q��@Q��@QX@Q&�@P��@P��@PbN@P  @O�@O�P@Ol�@OK�@O
=@N�+@N$�@N{@M�-@M`B@M�@L�/@L��@L�@Lj@L9X@K�F@KdZ@KdZ@Ko@J��@J�@I�@I��@IX@H�`@HĜ@H�@HbN@H �@G�w@G\)@G�@F��@F�y@F��@F$�@E�T@E��@E��@E?}@E/@D��@D(�@C�m@C�@CC�@C"�@B�H@B�!@Bn�@A�^@Ahs@AX@@��@@��@@�@@A�@@b@?�@?��@?�w@?��@?;d@>�R@>@=`B@=�@<��@<�j@<��@<Z@<(�@;��@;ƨ@;��@;o@:��@:M�@9��@9�#@9��@9��@9�7@9%@8��@8�u@8�@8r�@8A�@8b@7�;@7�w@7�@7�P@7�P@7|�@7;d@7
=@6�@6V@6{@5�T@5�-@5��@4��@4�@4Z@4�@3��@3�m@3��@3S�@3@2��@2~�@2-@1�7@0��@0�`@0�9@0��@0��@0�u@0bN@0 �@/�;@/�P@/\)@/�@.��@.�y@.��@.E�@.@-��@-�-@-��@-�@-O�@-�@,��@,��@,�@,��@,z�@,9X@,(�@,1@+ƨ@+S�@+33@+"�@*��@*�!@*=q@*-@*-@*�@)��@)�^@)x�@)G�@)�@(�`@(�9@(r�@(A�@(1'@(1'@'�@'�@'\)@'�@'
=@&�y@&ȴ@&��@&�+@&ff@&5?@&{@%�@%��@%�-@%�h@%`B@%/@%/@%V@$�@$Z@$1@#��@#�F@#��@#�@#t�@#dZ@#C�@"�H@"~�@"n�@"=q@!�#@!��@!�7@!&�@ ��@ ��@ �`@ Ĝ@ Ĝ@ ��@ �u@ r�@ Q�@   @�;@��@�w@��@�P@�@�R@��@�+@E�@{@@�@�T@@�@/@�@�j@��@9X@1@�m@�
@�
@�F@��@��@�@�@�@33@�@�@��@��@M�@J@��@��@�7@�@%@�@%@�u@r�@1'@�@�w@l�@�@�@v�@�@@@�-@�h@�@�@p�@`B@O�@O�@?}@?}@?}@V@��@�D@Z@�
@��@dZ@o@�!@n�@-@�@J@J@�@�7@hs@hs@X@��@�`@Ĝ@��@r�@bN@Q�@A�@1'@1'@ �@b@�@�@�y@��@v�@V@E�@5?@$�@{@@��@�-@p�@/@V@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��/A��A���A���A�ȴA���A��A��TA��mA��TA��A��A��A��A��mA��A��A��A��A��A���A���A���A���A��DA��`A��A�Q�A�9XA�|�A��7A�|�A�ffA�E�A�=qA�7LA�dZA�VA��yA���A��yA��yA��;A��wA��
A��A��A��A���A�x�A���A�A�XA��#A�bNA���A�ZA�dZA��A�z�A���A�bNA�"�A��7A�7LA���A���A��A��A���A��A��uA��A�9XA�M�A�A�C�A�=qA���A�1A�O�A�5?A�;dA�bA���A�x�A�VA��A�{A�33A�ZA���A�r�A�A��`A���A��A�M�A��A��#A���A���A��\A�
=A��A��-A���A��mA��A���A���A��
A���A�VA���A��\A}p�Ayl�Av�yAs�
Aq\)Anv�AlȴAk33Ah�RAgl�Ad5?AbVA_G�A^�\A]AZ�HAY�PAWO�AU�mASƨAQhsAPz�AO��AO/ANjAL~�AJAH�`AHE�AGl�AFZAD��ACC�AB�`AB�A@�DA?�wA>�9A=A;�wA:��A:�DA8ĜA7�mA7VA65?A5|�A3��A2I�A1;dA.�A-�
A,^5A,$�A,�DA+��A*n�A)�A(r�A'��A&ZA$bNA"1'A �!AƨA�A7LA�;A|�A��A�A�-A��A��A33A��A�mA|�A�A�7A=qA��A%A�9AffA�#A�7A%A�#A�\A
�jA	dZAA�A�A{A&�Ar�A1A�-A�RA��A
=A bN@�l�@���@��9@�t�@��@��w@���@���@�Q�@�;d@�7L@�"�@�7L@�@ꗍ@�7L@旍@��;@�+@�v�@�&�@��@�S�@�-@݁@܋D@ۮ@ڇ+@�=q@ٲ-@�7L@�9X@�\)@���@�/@�C�@ѩ�@���@�9X@ϥ�@���@���@�Ĝ@�M�@�hs@ȓu@�7L@ɡ�@��@��T@�`B@���@�9X@�o@�n�@�=q@š�@���@§�@�V@���@��F@�ȴ@�v�@�E�@��@��7@�&�@��`@��@�dZ@�V@�%@�9X@�ƨ@���@�
=@��@��@��@�Ĝ@�  @�C�@��!@�n�@��#@��@�1'@�l�@���@�x�@��@���@�C�@�@��R@�-@���@�G�@��`@�(�@��w@��@�C�@��R@�{@��7@�/@�z�@��
@�\)@�
=@���@�n�@�@���@�x�@��@��@���@��j@�r�@�1'@�1@��m@�ƨ@�t�@�dZ@�K�@�K�@�K�@�;d@�o@��@���@���@��+@�5?@��^@�O�@��u@�Z@�(�@��m@��@���@��@���@�x�@�p�@�%@��j@��@�z�@��@�ƨ@�l�@�o@���@���@��+@�~�@�ff@�$�@��@���@��-@���@��@�p�@�`B@�7L@��9@���@�z�@�(�@��
@��@�K�@�33@��H@��!@��\@���@���@��+@�v�@�V@�5?@���@��T@��^@�O�@�/@�&�@���@��9@��j@��9@�r�@� �@�I�@�A�@��m@��P@�S�@���@��y@���@���@��!@���@�E�@�J@�@���@�x�@�?}@��/@���@��@��@�%@�  @���@��P@�dZ@�S�@�33@��@��y@��H@���@��!@���@�^5@�M�@�=q@�J@��@���@�@��^@���@���@��h@�X@��@�Ĝ@��D@�z�@�r�@�9X@�(�@��m@�t�@�;d@��@��R@���@��+@�V@�J@��@���@���@�p�@�O�@�7L@���@��@��`@���@��@��D@�z�@�bN@� �@��;@���@��@��P@�|�@�S�@�33@��@���@�ff@�=q@�{@���@��@���@���@��@�/@���@�bN@� �@�b@� �@�w@+@~��@}�T@}`B@}�@|j@|(�@|1@{ƨ@{��@{S�@z��@z^5@z=q@zJ@zJ@y�@y�7@xĜ@xr�@w�;@w�@v�+@v5?@v{@u��@u/@t�j@s�m@s33@r�@r��@r^5@r=q@r�@q�7@q��@qhs@pA�@o�w@ol�@n�@n$�@n@m@m�-@m�h@mO�@l��@l��@lZ@kƨ@kC�@k"�@k@j�H@j��@j~�@j�@i��@i�^@ix�@iG�@i&�@i�@h�u@hA�@g|�@g�@f�@fv�@fE�@fE�@fE�@f{@e�-@e?}@d��@d�@d��@d�j@d��@d9X@c�
@ct�@c"�@co@b��@b~�@bM�@a�^@ahs@a&�@`Ĝ@`r�@`b@_�w@_
=@^��@^V@^$�@]��@]p�@\��@\��@\��@[�m@[dZ@[33@Z��@Z��@Zn�@Y��@Y�7@YG�@X��@Xb@W�@W��@W|�@Wl�@W;d@V�y@V��@VE�@U��@U?}@T�@T�D@TI�@T�@St�@SdZ@R�@R~�@RM�@Q��@Q��@Q��@QX@Q&�@P��@P��@PbN@P  @O�@O�P@Ol�@OK�@O
=@N�+@N$�@N{@M�-@M`B@M�@L�/@L��@L�@Lj@L9X@K�F@KdZ@KdZ@Ko@J��@J�@I�@I��@IX@H�`@HĜ@H�@HbN@H �@G�w@G\)@G�@F��@F�y@F��@F$�@E�T@E��@E��@E?}@E/@D��@D(�@C�m@C�@CC�@C"�@B�H@B�!@Bn�@A�^@Ahs@AX@@��@@��@@�@@A�@@b@?�@?��@?�w@?��@?;d@>�R@>@=`B@=�@<��@<�j@<��@<Z@<(�@;��@;ƨ@;��@;o@:��@:M�@9��@9�#@9��@9��@9�7@9%@8��@8�u@8�@8r�@8A�@8b@7�;@7�w@7�@7�P@7�P@7|�@7;d@7
=@6�@6V@6{@5�T@5�-@5��@4��@4�@4Z@4�@3��@3�m@3��@3S�@3@2��@2~�@2-@1�7@0��@0�`@0�9@0��@0��@0�u@0bN@0 �@/�;@/�P@/\)@/�@.��@.�y@.��@.E�@.@-��@-�-@-��@-�@-O�@-�@,��@,��@,�@,��@,z�@,9X@,(�@,1@+ƨ@+S�@+33@+"�@*��@*�!@*=q@*-@*-@*�@)��@)�^@)x�@)G�@)�@(�`@(�9@(r�@(A�@(1'@(1'@'�@'�@'\)@'�@'
=@&�y@&ȴ@&��@&�+@&ff@&5?@&{@%�@%��@%�-@%�h@%`B@%/@%/@%V@$�@$Z@$1@#��@#�F@#��@#�@#t�@#dZ@#C�@"�H@"~�@"n�@"=q@!�#@!��@!�7@!&�@ ��@ ��@ �`@ Ĝ@ Ĝ@ ��@ �u@ r�@ Q�@   @�;@��@�w@��@�P@�@�R@��@�+@E�@{@@�@�T@@�@/@�@�j@��@9X@1@�m@�
@�
@�F@��@��@�@�@�@33@�@�@��@��@M�@J@��@��@�7@�@%@�@%@�u@r�@1'@�@�w@l�@�@�@v�@�@@@�-@�h@�@�@p�@`B@O�@O�@?}@?}@?}@V@��@�D@Z@�
@��@dZ@o@�!@n�@-@�@J@J@�@�7@hs@hs@X@��@�`@Ĝ@��@r�@bN@Q�@A�@1'@1'@ �@b@�@�@�y@��@v�@V@E�@5?@$�@{@@��@�-@p�@/@V@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B

=B

=B
	7B
1B
	7B
	7B
	7B
DB
JB
DB
PB
PB
PB
PB
PB
VB
VB
\B
\B
\B
hB
�B
�B
-B
�BDBT�BgmB�B��B��B�B�B�B�B�-B��B�sB�;B�B�B��B��B1BbB�B"�B�B1'B9XB0!B0!BA�BM�BT�BW
B[#BaHBiyB\)BW
BR�BQ�BN�BG�BI�Bk�Bm�BdZBI�B?}BJ�BH�B6FB)�B#�B�BuB	7B��B�B�#B��BB�'B��B�DB�Bw�Bk�BN�BE�B@�B8RB49B.B"�B�B"�B(�B�BuBoBVB
��B
�5B
�jB
�!B
��B
��B
��B
�hB
�DB
p�B
XB
=qB
�B	��B	�TB	ǮB	�?B	��B	�DB	�B	l�B	e`B	VB	I�B	;dB	49B	-B	�B	�B	PB	B��B�B�mB�NB�;B�)B�#B��B��B��BɺBƨBĜB�jB�^B�XB�-B�'B�B��B��B��B��B��B��B�uB�\B�PB�By�Bt�Bn�Bk�BhsBhsBr�Br�Bo�Bn�Bk�BhsBe`BffBZBQ�BO�BO�BJ�BH�BF�BF�BC�BB�BA�B?}B=qB<jB:^B9XB8RB7LB5?B49B49B33B33B33B2-B2-B2-B1'B1'B/B.B-B.B,B-B,B,B,B-B+B)�B)�B+B,B,B+B,B,B,B/B1'B/B/B49B6FB6FB8RB8RB6FB33B5?B8RB9XB9XB;dB=qB>wB=qB@�BB�BC�BB�BB�BB�BC�BD�BA�B?}B?}BA�BC�B@�B?}BF�BA�BB�BI�BP�BT�BZBbNBcTBbNBaHBaHB`BB`BB_;B^5B_;B`BB_;B_;BaHBaHBaHBaHBbNBbNBbNBbNBe`Bk�Bq�Bs�Bt�Bu�Bw�Bx�Bw�Bx�B|�B~�B� B~�B� B�B�B�B�+B�=B�\B�uB��B��B��B��B��B��B��B��B�B�'B�-B�3B�FB�dB�}BBɺB��B�B�B�5B�BB�ZB�sB�yB�B�B�B�B��B��B��B��B��B	B	B	%B	+B	+B	+B		7B	JB	PB	bB	oB	�B	�B	�B	!�B	#�B	#�B	$�B	&�B	(�B	.B	0!B	2-B	2-B	49B	9XB	:^B	<jB	=qB	A�B	C�B	F�B	J�B	M�B	N�B	O�B	O�B	Q�B	Q�B	R�B	S�B	VB	XB	ZB	[#B	]/B	bNB	cTB	dZB	gmB	jB	m�B	o�B	p�B	r�B	s�B	u�B	x�B	z�B	}�B	~�B	�B	�B	�B	�B	�+B	�7B	�=B	�DB	�PB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�FB	�RB	�^B	�dB	�jB	�wB	B	ÖB	ÖB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�;B	�BB	�HB	�HB	�NB	�NB	�TB	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B

=B

=B

=B

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
\B
\B
bB
bB
bB
hB
hB
oB
oB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
#�B
#�B
#�B
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
'�B
'�B
(�B
(�B
)�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
+B
+B
,B
,B
,B
-B
-B
-B
.B
-B
.B
.B
.B
/B
/B
/B
0!B
1'B
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
33B
33B
33B
33B
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
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
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
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
B�B
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
D�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
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
I�B
I�B
I�B
J�B
J�B
J�B
J�B
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
K�B
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
N�B
N�B
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
Q�B
R�B
R�B
R�B
R�B
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
XB
YB
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
]/B
]/B
]/B
]/B
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
`BB
`BB
aHB
aHB
aHB
aHB
aHB
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
`BB
`BB
aHB
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
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
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
hsB
iyB
hsB
iyB
iyB
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
k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B

	B

	B
	B
�B
	B
	B
	B
)B
0B
)B
B
B
B
B
B
<B
"B
(B
(B
(B
4B
_B
xB
,�B
��B)BT�Bg8B��B��B��B��B��B��B��B��B��B�>B�B�KB�oB��B��B�B.B_B"�B�B0�B9$B/�B/�BAUBM�BT�BV�BZ�BaBiDB[�BV�BR�BQ�BN�BGzBI�BkQBm]Bd&BI�B?HBJ�BH�B6B)�B#�B�B@B	B��B�WB��BΥB�[B��B�~B�B��Bw�BkQBN�BEmB@OB8B4B-�B"�BeB"�B(�BSB@B:B"B
��B
�B
�6B
��B
��B
��B
�YB
�4B
�B
poB
W�B
=<B
xB	��B	� B	�zB	�B	��B	�B	��B	l=B	e,B	U�B	I�B	;0B	4B	,�B	�B	�B	B	�B��B�]B�8B�B��B��B��B��BϫB̘B�lB�YB�MB�B�*B�$B��B��B��B��B��B��B�xB�QB�SB�&B�(B�B��By�Bt�BnIBkQBh>Bh$Br|Br|BoiBncBkQBh>BeBf2BY�BQ�BO�BO�BJ�BH�BFYBFtBCGBB[BA;B?HB="B<6B:*B9$B8B6�B4�B4B4B2�B2�B2�B1�B1�B1�B0�B0�B.�B-�B,�B-�B+�B,�B+�B+�B+�B,�B*�B)�B)�B*�B+�B+�B*�B+�B+�B+�B.�B0�B.�B.�B4B5�B6B8B8B5�B2�B5B8B9	B9$B;0B=<B>(B="B@4BBABCGBBABBABBABCGBDgBAUB?HB?.BAUBCGB@OB?.BFtBA;BB[BI�BP�BT�BY�Ba�Bc BbBaB`�B`B`B_B^B^�B_�B^�B^�B`�B`�BaB`�BbBa�Ba�Ba�BeBkQBq[Bs�BtnButBw�Bx�Bw�Bx�B|�B~�B�B~�B�B��B��B��B��B�	B�(B�&B�EB�dB�~B��B�|B��B��B��B��B��B��B��B�B�0B�.B�ABɆBЗBյB��B�B�B�&B�>B�DB�WB�OB�UB�vB�B��B��B��B��B	�B	�B	�B	�B	�B	�B	�B	B	B	.B	 B	2B	kB	�B	!|B	#�B	#�B	$�B	&�B	(�B	-�B	/�B	1�B	1�B	3�B	9	B	:B	<6B	="B	A;B	CGB	FYB	J�B	M�B	N�B	O�B	O�B	Q�B	Q�B	R�B	S�B	U�B	W�B	Y�B	Z�B	\�B	bB	cB	dB	g8B	jKB	mCB	oOB	poB	raB	shB	utB	x�B	z�B	}�B	~�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�MB	�EB	�EB	�QB	�WB	�~B	�jB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�BB	�[B	�GB	�aB	�MB	�MB	�mB	�YB	�_B	ȀB	ɆB	ɆB	ɆB	�rB	�rB	˒B	�xB	̘B	ЗB	ѝB	ңB	өB	ԯB	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�,B	�B	�B	�B	�8B	�$B	�$B	�>B	�$B	�DB	�0B	�6B	�6B	�WB	�=B	�WB	�=B	�=B	�IB	�OB	�oB	�[B	�vB	�vB	�[B	�aB	�B	�hB	�hB	�B	�nB	�nB	�nB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
	B
	�B

	B
	�B

	B
	�B
	�B
	�B

�B
B
�B
�B
B
B
B
B
"B
"B
B
B
(B
(B
B
B
.B
B
B
 B
 B
@B
&B
FB
,B
,B
,B
,B
2B
2B
2B
9B
SB
SB
9B
SB
YB
?B
YB
?B
_B
_B
KB
eB
QB
kB
QB
WG�O�B
]B
]B
~B
~B
dB
jB
jB
jB
pB
pB
 vB
 vB
 vB
!|B
!|B
!|B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
#�B
#�B
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
'�B
'�B
(�B
(�B
)�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
,�B
-�B
,�B
-�B
-�B
-�B
.�B
.�B
.�B
/�B
0�B
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
2�B
2�B
2�B
2�B
3�B
3�B
4B
5B
5B
6B
5�B
5�B
5�B
6B
7B
7B
7B
8B
8B
9$B
9	B
9	B
9	B
:B
:B
:*B
:B
;B
;B
;0B
;B
;B
;0B
<B
="B
="B
="B
="B
="B
>(B
>BB
>BB
>BB
>BB
>BB
?.B
?.B
@OB
@4B
@4B
@OB
@4B
@4B
AUB
A;B
AUB
A;B
A;B
BAB
BAB
BAB
BAB
B[B
BAB
B[B
BAB
BAB
CaB
CaB
CaB
CGB
CGB
CGB
CaB
DMB
DMB
DMB
ESB
EmB
ESB
ESB
ESB
ESB
ESB
ESB
FtB
FYB
G_B
GzB
GzB
G_B
G_B
G_B
G_B
G_B
G_B
HfB
HfB
I�B
IlB
I�B
IlB
I�B
IlB
JrB
JrB
J�B
JrB
JrB
JrB
JrB
KxB
KxB
KxB
KxB
KxB
KxB
K�B
KxB
L~B
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
N�B
N�B
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
Q�B
R�B
R�B
R�B
R�B
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
W�B
X�B
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
Z�B
Z�B
Z�B
Z�B
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
\�B
^B
^�B
^�B
^�B
_B
_B
^�B
_B
^�B
_�B
_�B
_�B
`B
_�B
`�B
aB
aB
aB
`�B
`B
`B
_�B
`B
_�B
_�B
`B
_�B
_�B
_�B
`B
`B
_�B
`B
`�B
`�B
`�B
aB
aB
a�B
bB
a�B
c B
c B
cB
c B
c B
dB
dB
d&B
d&B
dB
dB
e,B
eB
e,B
f2B
fB
f2B
f2B
fB
fB
f2B
fB
f2B
gB
h>B
i*B
h>B
iDB
iDB
i*B
i*B
iDB
i*B
j0B
jKB
j0B
j0B
jKB
k6B
kQB
k6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.53(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903100034232019031000342320190310003423201903110025022019031100250220190311002502JA  ARFMdecpA19c                                                                20190305003629  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190304153731  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190304153733  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190304153733  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190304153734  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190304153734  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190304153734  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190304153734  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190304153734  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190304153734                      G�O�G�O�G�O�                JA  ARUP                                                                        20190304155551                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190304153447  CV  JULD            G�O�G�O�F�`�                JM  ARGQJMQC2.0                                                                 20190304153447  CV  JULD_LOCATION   G�O�G�O�F�a                JM  ARGQJMQC2.0                                                                 20190304153447  CV  LATITUDE        G�O�G�O�A�=q                JM  ARGQJMQC2.0                                                                 20190304153447  CV  LONGITUDE       G�O�G�O���H                JM  ARSQJMQC2.0                                                                 20190305000000  CF  PSAL_ADJUSTED_QCD�� D�� G�O�                JM  ARCAJMQC2.0                                                                 20190309153423  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190309153423  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190310152502  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                