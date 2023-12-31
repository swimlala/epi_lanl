CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-02-28T09:36:23Z creation;2019-02-28T09:36:27Z conversion to V3.1;2019-12-23T06:06:14Z update;     
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
_FillValue                 �  I(   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �x   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �@   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �@   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �@   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �@   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190228093623  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_128                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ث��P�1   @ثb� @8*^5?|��c4ě��T1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D���D�@ DЀ D�� D�  D�@ Dу3D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��\@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Du!�Du��Dv!�Dv��Dw!�Dw��Dx!�Dx��Dy!�Dy��Dz!�Dz��D{!�D{��D|!�D|��D}!�D}��D~!�D~��D!�D��D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D��)D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D�D���D��D�P�DÐ�D���D��D�P�DĐ�D���D��D�P�DŐ�D���D��D�P�DƐ�D���D��D�P�Dǐ�D���D��D�P�DȐ�D���D��D�P�Dɐ�D���D��D�P�Dʐ�D���D��D�P�Dː�D���D��D�P�D̐�D���D��D�P�D͐�D���D��D�P�Dΐ�D���D��D�P�Dϐ�D���D��D�P�DА�D���D��D�P�Dє)D���D��D�P�DҐ�D���D��D�P�DӐ�D���D��D�P�DԐ�D���D��D�P�DՐ�D���D��D�P�D֐�D���D��D�P�Dא�D���D��D�P�Dؐ�D���D��D�P�Dِ�D���D��D�P�Dڐ�D���D��D�P�Dې�D���D��D�P�Dܐ�D���D��D�P�Dݐ�D���D��D�P�Dސ�D���D��D�P�Dߐ�D���D��D�P�D���D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D��)D�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��7A��7A��A�n�A�p�A�z�A�v�A��A��DA�~�A��A���A���A���A���A���A��!A��9A���A��`A��A�ƨA���A��uA�r�A�9XA�+A�33A�33A��A�  A���A��A��yA���A��^A���A���A�r�A�Q�A�K�A�E�A�7LA�-A�"�A��A�oA���A�~�A���A���A�^5A�ĜA��A��A��A��9A���A��\A���A�/A�`BA�bA���A�x�A��yA���A�+A��!A���A�"�A��PA��-A��mA�+A�l�A�7LA��A��jA��A�A�~�A�C�A��A�5?A��wA��A�ZA�ȴA���A� �A�bA��9A�ffA���A�|�A�=qA��yA�5?A�|�A�JA�v�A�A�A���A��^A���A�TA~I�A}%A{��Ax�yAv�Ar~�Aop�AmS�AkAi�PAg�Ab��A`^5A^ȴA]�wA\~�AZr�AW��AVbNAU��AT-ARn�AP�ANz�AK"�AIt�AH�`AG+AE��AE/AD�uAC"�ABĜAA�A?A>�!A=�A;��A9/A6��A5��A5K�A4�jA3�;A3ƨA3`BA2�RA2ZA1�#A1p�A0�A/S�A-�#A,�RA+/A*�\A)�A)C�A'�;A&ĜA&JA$�A#�#A#��A#A!S�A �jA �\A ffA A��AffA�#A�/AbA��A��A�A�A��Ap�A33Av�A�A�RAt�AȴA=qA�mA��A�
A�RA�A��A/A�jA=qAAt�AS�A
-A�jAVA��AbNAA�A��Av�AbNA��A�DA��AoA r�@���@���@�@�r�@�ff@�G�@��@�-@�/@@�7@�9@�b@ꟾ@�X@�1@�l�@�ff@�hs@�D@��@���@�O�@���@��D@�t�@�-@�X@�Z@ە�@�33@�n�@�Ĝ@���@�^5@�V@�@�-@�j@�l�@�n�@͡�@�ƨ@��y@�^5@��@ǶF@�;d@Ɨ�@��@��@�V@Ĭ@Ý�@��@�@���@�r�@��@���@���@���@�o@��+@���@�&�@��@��/@�r�@��@�C�@�=q@��@��h@�7L@���@�Z@���@���@��7@�j@�Q�@��F@��R@�M�@�@��@�  @��@�v�@�-@��-@�x�@�?}@��9@��;@���@��@���@�ff@��@�@��^@�`B@��j@�1'@�  @��
@��@���@�dZ@�+@��@�ff@�5?@���@�`B@�?}@���@���@��D@���@���@���@�|�@�l�@�S�@�;d@�;d@�33@��@��\@�J@�@�O�@�V@�bN@�1@��@���@���@���@�|�@�t�@�33@���@�=q@���@��@�&�@���@��/@��/@���@�r�@�A�@�b@���@��P@�S�@�+@���@���@���@�^5@�M�@�$�@���@��7@�/@�%@���@�Ĝ@��D@��@��
@���@��P@�dZ@�;d@�@���@���@���@��@��R@�v�@�V@�M�@�M�@�E�@�5?@��@���@��#@��^@���@�hs@�/@���@��j@�Ĝ@���@���@���@���@��@��@�1@��
@���@�l�@��@��@��R@��+@�ff@�E�@�{@���@���@��@�?}@��@��/@�Ĝ@��j@�Z@�b@�|�@�K�@�"�@�o@�o@�o@��y@�5?@��@�=q@�5?@��#@���@���@���@��@�O�@��@��@���@��/@���@�Q�@���@�dZ@�\)@�K�@�;d@���@���@���@�~�@�M�@�-@���@��@��T@���@��h@�G�@��@��`@��u@�j@�I�@�(�@��
@��@���@�l�@�\)@�K�@�C�@�"�@��@��@��R@��+@�^5@��@��@�@�hs@�G�@��@��D@�Z@��@��@K�@~�y@~5?@}�h@}/@|��@|z�@{��@{C�@{@z��@zM�@z�@y��@y��@y��@y�7@yX@y%@xĜ@xA�@w�;@wK�@v��@vȴ@v5?@u@uO�@u�@tZ@tI�@t�@s��@sS�@s"�@r�@r~�@r-@q�7@q�@p�u@pbN@pA�@pb@o�w@o|�@o+@n�+@nV@m�T@m/@l�j@lj@k��@k�F@k�@kt�@k33@k"�@j�@j~�@i�@i�#@i��@iG�@hĜ@h��@hr�@h �@g��@g��@g
=@fE�@e�T@e�-@e/@d�j@d�@d�D@d1@c��@c��@cdZ@c"�@b�@b��@b��@b^5@b-@a��@a�@`�`@`��@`  @_�@_;d@^�R@^��@^�+@^E�@]�T@]�h@]?}@]V@\�@\Z@\9X@[�
@[��@[t�@[S�@[33@[@Z��@Z��@Z~�@Y��@YG�@X�`@XĜ@X��@Xr�@W�;@Wl�@V�+@V5?@U�@U@U�-@U�@U?}@T�@TI�@S�F@SS�@S33@R�H@R��@Rn�@R=q@R�@Q��@Q�@Q�@Q�#@Q��@Q%@P��@P �@O�;@O�w@Ol�@O�@N�y@N��@Nff@N$�@M��@M�h@Mp�@M`B@MO�@MO�@MO�@M?}@M�@L�@LI�@K�m@Kƨ@K��@KdZ@KC�@K"�@J�@J�!@J~�@J=q@J-@J-@JJ@I��@I��@I��@I�@H��@H�@G�@Gl�@G;d@G+@G
=@F�@F�R@F�+@FE�@F@E@E�h@EO�@EV@D�@Dj@D1@C�F@CdZ@CC�@Co@B�!@B�\@Bn�@B=q@B�@A��@A�^@AX@A&�@A%@@��@@�`@@Ĝ@@bN@@b@?�w@?K�@>��@>�@>�R@>��@>v�@>$�@=�@=�T@=�-@=p�@<z�@<(�@;�F@;��@;C�@:�@:��@:=q@9��@9��@9�^@9��@97L@8�u@8Q�@8 �@7�@7�@7\)@7�@6�R@6ff@6@5�h@5/@4�@4�D@4I�@49X@4(�@3ƨ@3dZ@333@2��@2-@1��@1�#@1�^@1hs@1&�@0�`@0�u@0r�@0bN@01'@/�;@/�@/�P@/+@/�@.��@.��@.�+@.5?@-��@-�@-?}@,��@,��@,Z@,(�@+ƨ@+��@+t�@+S�@+"�@*�H@*��@*^5@)��@)�#@)��@)%@(�9@(bN@(bN@(bN@(bN@(b@'��@'|�@'K�@'
=@&ȴ@&�R@&��@&�+@&ff@&5?@&{@%�@%@%�h@%?}@$�@$��@$�@$�D@$Z@$(�@$1@#�
@#ƨ@#�@#t�@#dZ@#"�@#@"�!@"~�@"^5@"M�@"�@!��@!��@!�^@!��@!��@!x�@!G�@!&�@!%@ ��@ ��@ �@ A�@   @�@l�@;d@�@�y@�R@�R@�R@��@��@��@�+@ff@E�@5?@{@�T@�-@p�@`B@`B@`B@`B@?}@��@��@z�@j@9X@��@ƨ@��@S�@@��@�\@n�@n�@-@�@�#@�^@��@hs@�@�`@�u@Q�@b@�@�P@
=@��@�y@�@ȴ@�R@��@��@�+@ff@V@@��@p�@?}@��@�j@�D@�D@�D@�D@z�@j@��@�F@t�@S�@@��@��@~�@^5@-@�#@�^@��@��@�7@x�@x�@hs@hs@X@G�@%@��@��@bN@�@�w@��@|�@l�@+@��@�y@ȴ@v�@V@5?@��@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��7A��7A��A�n�A�p�A�z�A�v�A��A��DA�~�A��A���A���A���A���A���A��!A��9A���A��`A��A�ƨA���A��uA�r�A�9XA�+A�33A�33A��A�  A���A��A��yA���A��^A���A���A�r�A�Q�A�K�A�E�A�7LA�-A�"�A��A�oA���A�~�A���A���A�^5A�ĜA��A��A��A��9A���A��\A���A�/A�`BA�bA���A�x�A��yA���A�+A��!A���A�"�A��PA��-A��mA�+A�l�A�7LA��A��jA��A�A�~�A�C�A��A�5?A��wA��A�ZA�ȴA���A� �A�bA��9A�ffA���A�|�A�=qA��yA�5?A�|�A�JA�v�A�A�A���A��^A���A�TA~I�A}%A{��Ax�yAv�Ar~�Aop�AmS�AkAi�PAg�Ab��A`^5A^ȴA]�wA\~�AZr�AW��AVbNAU��AT-ARn�AP�ANz�AK"�AIt�AH�`AG+AE��AE/AD�uAC"�ABĜAA�A?A>�!A=�A;��A9/A6��A5��A5K�A4�jA3�;A3ƨA3`BA2�RA2ZA1�#A1p�A0�A/S�A-�#A,�RA+/A*�\A)�A)C�A'�;A&ĜA&JA$�A#�#A#��A#A!S�A �jA �\A ffA A��AffA�#A�/AbA��A��A�A�A��Ap�A33Av�A�A�RAt�AȴA=qA�mA��A�
A�RA�A��A/A�jA=qAAt�AS�A
-A�jAVA��AbNAA�A��Av�AbNA��A�DA��AoA r�@���@���@�@�r�@�ff@�G�@��@�-@�/@@�7@�9@�b@ꟾ@�X@�1@�l�@�ff@�hs@�D@��@���@�O�@���@��D@�t�@�-@�X@�Z@ە�@�33@�n�@�Ĝ@���@�^5@�V@�@�-@�j@�l�@�n�@͡�@�ƨ@��y@�^5@��@ǶF@�;d@Ɨ�@��@��@�V@Ĭ@Ý�@��@�@���@�r�@��@���@���@���@�o@��+@���@�&�@��@��/@�r�@��@�C�@�=q@��@��h@�7L@���@�Z@���@���@��7@�j@�Q�@��F@��R@�M�@�@��@�  @��@�v�@�-@��-@�x�@�?}@��9@��;@���@��@���@�ff@��@�@��^@�`B@��j@�1'@�  @��
@��@���@�dZ@�+@��@�ff@�5?@���@�`B@�?}@���@���@��D@���@���@���@�|�@�l�@�S�@�;d@�;d@�33@��@��\@�J@�@�O�@�V@�bN@�1@��@���@���@���@�|�@�t�@�33@���@�=q@���@��@�&�@���@��/@��/@���@�r�@�A�@�b@���@��P@�S�@�+@���@���@���@�^5@�M�@�$�@���@��7@�/@�%@���@�Ĝ@��D@��@��
@���@��P@�dZ@�;d@�@���@���@���@��@��R@�v�@�V@�M�@�M�@�E�@�5?@��@���@��#@��^@���@�hs@�/@���@��j@�Ĝ@���@���@���@���@��@��@�1@��
@���@�l�@��@��@��R@��+@�ff@�E�@�{@���@���@��@�?}@��@��/@�Ĝ@��j@�Z@�b@�|�@�K�@�"�@�o@�o@�o@��y@�5?@��@�=q@�5?@��#@���@���@���@��@�O�@��@��@���@��/@���@�Q�@���@�dZ@�\)@�K�@�;d@���@���@���@�~�@�M�@�-@���@��@��T@���@��h@�G�@��@��`@��u@�j@�I�@�(�@��
@��@���@�l�@�\)@�K�@�C�@�"�@��@��@��R@��+@�^5@��@��@�@�hs@�G�@��@��D@�Z@��@��@K�@~�y@~5?@}�h@}/@|��@|z�@{��@{C�@{@z��@zM�@z�@y��@y��@y��@y�7@yX@y%@xĜ@xA�@w�;@wK�@v��@vȴ@v5?@u@uO�@u�@tZ@tI�@t�@s��@sS�@s"�@r�@r~�@r-@q�7@q�@p�u@pbN@pA�@pb@o�w@o|�@o+@n�+@nV@m�T@m/@l�j@lj@k��@k�F@k�@kt�@k33@k"�@j�@j~�@i�@i�#@i��@iG�@hĜ@h��@hr�@h �@g��@g��@g
=@fE�@e�T@e�-@e/@d�j@d�@d�D@d1@c��@c��@cdZ@c"�@b�@b��@b��@b^5@b-@a��@a�@`�`@`��@`  @_�@_;d@^�R@^��@^�+@^E�@]�T@]�h@]?}@]V@\�@\Z@\9X@[�
@[��@[t�@[S�@[33@[@Z��@Z��@Z~�@Y��@YG�@X�`@XĜ@X��@Xr�@W�;@Wl�@V�+@V5?@U�@U@U�-@U�@U?}@T�@TI�@S�F@SS�@S33@R�H@R��@Rn�@R=q@R�@Q��@Q�@Q�@Q�#@Q��@Q%@P��@P �@O�;@O�w@Ol�@O�@N�y@N��@Nff@N$�@M��@M�h@Mp�@M`B@MO�@MO�@MO�@M?}@M�@L�@LI�@K�m@Kƨ@K��@KdZ@KC�@K"�@J�@J�!@J~�@J=q@J-@J-@JJ@I��@I��@I��@I�@H��@H�@G�@Gl�@G;d@G+@G
=@F�@F�R@F�+@FE�@F@E@E�h@EO�@EV@D�@Dj@D1@C�F@CdZ@CC�@Co@B�!@B�\@Bn�@B=q@B�@A��@A�^@AX@A&�@A%@@��@@�`@@Ĝ@@bN@@b@?�w@?K�@>��@>�@>�R@>��@>v�@>$�@=�@=�T@=�-@=p�@<z�@<(�@;�F@;��@;C�@:�@:��@:=q@9��@9��@9�^@9��@97L@8�u@8Q�@8 �@7�@7�@7\)@7�@6�R@6ff@6@5�h@5/@4�@4�D@4I�@49X@4(�@3ƨ@3dZ@333@2��@2-@1��@1�#@1�^@1hs@1&�@0�`@0�u@0r�@0bN@01'@/�;@/�@/�P@/+@/�@.��@.��@.�+@.5?@-��@-�@-?}@,��@,��@,Z@,(�@+ƨ@+��@+t�@+S�@+"�@*�H@*��@*^5@)��@)�#@)��@)%@(�9@(bN@(bN@(bN@(bN@(b@'��@'|�@'K�@'
=@&ȴ@&�R@&��@&�+@&ff@&5?@&{@%�@%@%�h@%?}@$�@$��@$�@$�D@$Z@$(�@$1@#�
@#ƨ@#�@#t�@#dZ@#"�@#@"�!@"~�@"^5@"M�@"�@!��@!��@!�^@!��@!��@!x�@!G�@!&�@!%@ ��@ ��@ �@ A�@   @�@l�@;d@�@�y@�R@�R@�R@��@��@��@�+@ff@E�@5?@{@�T@�-@p�@`B@`B@`B@`B@?}@��@��@z�@j@9X@��@ƨ@��@S�@@��@�\@n�@n�@-@�@�#@�^@��@hs@�@�`@�u@Q�@b@�@�P@
=@��@�y@�@ȴ@�R@��@��@�+@ff@V@@��@p�@?}@��@�j@�D@�D@�D@�D@z�@j@��@�F@t�@S�@@��@��@~�@^5@-@�#@�^@��@��@�7@x�@x�@hs@hs@X@G�@%@��@��@bN@�@�w@��@|�@l�@+@��@�y@ȴ@v�@V@5?@��@�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�+B�%B�B�B�B�B�B�B�%B�B�B�JB�=B�DB�1B�1B�\B�uB��B��B�B�/B�;B�HB�fB�B�B�B�B��B��B��B��B��B��B��B��BB{B�B�B�B�B!�B$�B&�B(�B.B<jBF�BJ�BM�BN�BS�BZBcTBffBaHBYBW
BS�BM�BI�BE�BB�B?}B7LB+B(�B+B'�B$�B �B�BbB	7B+BB  B��B�B�ZB�B�dB�'B�B��B��B�Br�BhsBS�BH�B:^BJBB
��B
��B
�B
��B
�B
�oB
|�B
v�B
gmB
H�B
+B
�B
bB
B	�B	�;B	��B	��B	�{B	�1B	|�B	w�B	YB	C�B	9XB	0!B	'�B	�B	DB	B��B��B�B�sB�B�^B�'B�!B�FB�RB�LB�LB�'B�B�B��B��B��B�uB�+Bw�Bu�Bs�Bs�Bp�Bo�Bn�Bl�Bk�BjBiyBhsBffBe`BaHB^5B\)B[#BZBYBS�BR�BQ�BN�BM�BP�BO�BN�BM�BM�BL�BL�BJ�BK�BJ�BI�BH�BH�BF�BC�BB�B@�B?}B=qB;dB8RB9XB6FB6FB49B6FB33B5?B33B2-B2-B2-B2-B1'B0!B0!B2-B.B/B.B.B-B-B,B,B+B+B)�B)�B'�B(�B(�B+B.B-B,B+B-B-B/B33B0!B0!B1'B0!B.B/B5?B6FB8RB9XB<jB;dB;dB:^B:^B:^B<jB>wB>wB?}B@�BD�BD�BB�BB�BD�BC�BC�BA�BC�BE�BI�BG�BH�BI�BJ�BM�BN�BN�BW
B\)BVBS�BVBYBYBXB]/B_;B_;BaHBcTBcTBe`BhsBhsBjBk�Bl�Bn�Bp�Bs�Bt�Bv�Bx�Bx�Bz�B}�B�B�B�%B�%B�+B�=B�DB�DB�hB��B��B��B��B��B��B��B��B�B�!B�3B�FB�RB�^B�^B�jB�wBBƨBȴB��B��B��B��B��B�B�;B�HB�`B�mB�B�B�B��B��B��B��B	B	B	B	%B	+B	1B	DB	PB	\B	hB	{B	�B	�B	�B	�B	 �B	!�B	&�B	&�B	&�B	(�B	+B	-B	.B	1'B	5?B	:^B	:^B	:^B	;dB	=qB	?}B	A�B	C�B	E�B	G�B	H�B	J�B	J�B	L�B	L�B	M�B	O�B	R�B	VB	YB	[#B	]/B	]/B	_;B	bNB	e`B	gmB	gmB	iyB	jB	m�B	m�B	m�B	m�B	o�B	r�B	u�B	w�B	w�B	x�B	x�B	y�B	|�B	� B	�B	�%B	�1B	�=B	�PB	�PB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�9B	�?B	�FB	�LB	�RB	�RB	�dB	�^B	�^B	�dB	�jB	�wB	��B	��B	��B	B	ĜB	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�)B	�)B	�)B	�/B	�;B	�;B	�BB	�;B	�;B	�BB	�HB	�HB	�HB	�HB	�TB	�`B	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
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
B
B
%B
+B
+B
+B
+B
1B
1B
1B
1B
	7B

=B

=B
DB
DB
DB
DB
JB
JB
JB
PB
PB
VB
VB
\B
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
oB
uB
oB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
#�B
#�B
#�B
#�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
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
)�B
)�B
)�B
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
.B
.B
.B
.B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
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
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
7LB
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
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
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
=qB
>wB
>wB
?}B
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
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
O�B
O�B
O�B
O�B
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
R�B
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
]/B
^5B
^5B
^5B
^5B
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
ffB
ffB
ffB
ffB
gmB
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
jB
jB
jB
jB
jB
jB
jB
jB
k�B
jB
k�B
k�B
k�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B�B�B�B��B��B��B��B��B�B�	B�B��B��B�(B�@B�qB��B��B�B�!B�-B�2B�cB�cB�oB�|B��B��B��B��B��B��B��B��B�BFBeB�BqB~B!�B$�B&�B(�B-�B<6BF�BJ�BM�BN�BS�BY�Bc Bf2BaBX�BV�BS�BM�BI�BEmBB[B?HB7B*�B(�B*�B'�B$�B �B_B.B	B�B�B��B��B�WB�&B��B�0B��B��B��B�MB��Br|Bh>BS�BH�B:*BB�B
��B
��B
�QB
��B
��B
�:B
|�B
v�B
g8B
H�B
*�B
qB
.B
�B	�B	�B	�UB	��B	�FB	��B	|�B	w�B	X�B	CaB	9$B	/�B	'�B	kB	B	 �B��B��B�IB�>BյB�B��B��B��B�B�B�B��B��B��B��B��B�jB�@B��Bw�ButBs�Bs�BpoBoiBncBlWBk6Bj0Bi*Bh>Bf2BeBaB^B[�BZ�BY�BX�BS�BR�BQ�BN�BM�BP�BO�BN�BM�BM�BL�BL�BJ�BKxBJ�BIlBH�BH�BFYBCGBB[B@OB?HB="B;B8B9	B5�B6B4B5�B2�B5B2�B1�B1�B1�B1�B0�B/�B/�B1�B-�B.�B-�B-�B,�B,�B+�B+�B*�B*�B)�B)�B'�B(�B(�B*�B-�B,�B+�B*�B,�B,�B.�B2�B/�B/�B0�B/�B-�B.�B4�B5�B8B9	B<6B;0B;B:*B:B:*B<6B>BB>(B?.B@4BDgBDgBB[BB[BDgBCaBCGBA;BCGBESBI�BGzBH�BIlBJ�BM�BN�BN�BV�B[�BU�BS�BU�BX�BX�BW�B\�B_B^�B`�BcBcBeBh>Bh>Bj0BkQBl=BnIBpUBshBtnBvzBx�Bx�Bz�B}�B��B��B��B��B��B��B��B�B�4B�EB�~B�vB��B��B��B��B��B��B��B��B��B�B�B�B�B�BB�[B�YB�fB˒B̈́B̈́BϑBԯB��B�B�B�,B�B�=B�[B�B��B��B��B��B	�B	�B	�B	�B	�B	�B	B	B	(B	B	,B	?B	xB	jB	�B	 vB	!|B	&�B	&�B	&�B	(�B	*�B	,�B	-�B	0�B	5B	:B	:B	:B	;B	=<B	?.B	A;B	CGB	ESB	G_B	H�B	JrB	JrB	L�B	L~B	M�B	O�B	R�B	U�B	X�B	Z�B	\�B	\�B	_B	bB	eB	gB	gB	iDB	j0B	m]B	m]B	mCB	mCB	oOB	raB	u�B	w�B	w�B	x�B	x�B	y�B	|�B	�B	��B	��B	��B	��B	�B	�B	�4B	�MB	�eB	�WB	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�0B	�6B	�(B	�OB	�4B	�OB	�[B	�MB	�GB	�MB	�mB	�SB	�SB	�tB	ȀB	ʌB	�xB	̈́B	ΊB	ΥB	ϑB	бB	ѷB	ѝB	ңB	յB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	� B	�,B	�B	�B	�$B	�DB	�DB	�KB	�6B	�6B	�WB	�]B	�CB	�CB	�CB	�CB	�IB	�IB	�IB	�cB	�OB	�oB	�UB	�UB	�[B	�[B	�aB	�B	�hB	�nB	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B

	B
	�B
B
B

�B

�B
B
�B
B
B
B
B
"B
B
B
B
.B
.B
B
B
B
B
.B
B
B
B
 B
&B
:B
&B
&B
@B
,B
,B
2B
2B
2B
2B
MB
2B
9B
9B
?B
EB
EB
_B
_B
EB
EB
KB
KB
kB
kB
kB
WB
WB
]B
]B
dB
dB
~B
dB
jB
jB
�B
pB
pB
�B
pB
 vB
 vB
 vB
 �B
 �B
 �B
 vB
 vB
!�B
!�B
"�B
"�B
"�B
#�B
#�B
#�B
#�B
%�B
%�B
%�B
%�B
%�B
%�B
&�B
&�B
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
)�B
)�B
)�B
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
-�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
/�B
/�B
/�B
/�B
0�B
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
2�B
2�B
2�B
3�B
5B
5B
4�B
4�B
4�B
5B
5B
5�B
6B
6B
6B
6�B
7B
6�B
6�B
6�B
8B
8B
8B
8B
8B
8B
8B
8B
9	B
9	B
9	B
:B
:B
:B
:*B
:B
:*B
;0B
;B
;0B
<6B
<B
<B
<6B
<B
<B
="B
="B
=<B
=<B
=<B
>BB
>(B
?HB
>(B
?HB
?.B
?.B
@OB
@OB
@OB
@4B
@4B
A;B
AUB
BAB
BAB
BAB
BAB
CGB
CGB
CGB
CaB
DgB
DMB
ESB
EmB
ESB
EmB
ESB
EmB
FtB
FYB
FYB
GzB
G_B
G_B
HfB
H�B
HfB
HfB
H�B
IlB
IlB
I�B
I�B
JrB
JrB
JrB
J�B
JrB
K�B
KxB
K�B
KxB
L~B
L~B
L�B
L~B
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
O�B
O�B
O�B
O�B
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
R�B
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
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
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
[�B
[�B
[�B
[�B
[�B
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
]�B
]�B
]�B
^�B
^�B
_B
_B
^�B
_�B
`B
`B
_�B
_�B
aB
aB
`�B
aB
bB
cB
c B
cB
cB
c B
cB
cB
c B
cB
c B
cB
d&B
d&B
d&B
dB
e,B
eB
eB
eB
eB
e,B
e,B
eB
fB
fB
f2B
fB
g8B
g8B
g8B
gB
gB
g8B
h>B
h$B
h>B
h>B
h$B
h>B
h>B
h$B
h$B
h$B
h>B
h>B
iDB
iDB
i*B
i*B
j0B
jKB
j0B
jKB
jKB
jKB
j0B
jKB
k6B
jKB
kQB
kQB
kQ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.53(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903050035192019030500351920190305003519201903060025592019030600255920190306002559JA  ARFMdecpA19c                                                                20190228183622  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190228093623  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190228093625  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190228093625  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190228093626  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190228093626  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190228093626  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190228093626  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190228093627  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190228093627                      G�O�G�O�G�O�                JA  ARUP                                                                        20190228095553                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190228153341  CV  JULD            G�O�G�O�F�Xv                JM  ARGQJMQC2.0                                                                 20190228153341  CV  JULD_LOCATION   G�O�G�O�F�X�                JM  ARGQJMQC2.0                                                                 20190228153341  CV  LATITUDE        G�O�G�O�A�M�                JM  ARGQJMQC2.0                                                                 20190228153341  CV  LONGITUDE       G�O�G�O���f                JM  ARCAJMQC2.0                                                                 20190304153519  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190304153519  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190305152559  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                