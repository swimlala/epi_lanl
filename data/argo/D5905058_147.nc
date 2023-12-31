CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-06-06T09:18:08Z creation;2019-06-06T09:18:11Z conversion to V3.1;2019-12-23T06:02:03Z update;     
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
_FillValue                 �  IH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M4   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  td   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �,   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ΄   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ސ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20190606091808  20200120031518  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_147                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؿV�� 1   @ؿWm�5 @7��	��b����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @N�R@���@���Az�A(z�AHz�Ahz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�\B�\B��)B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*nC,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Du!�Du��Dv!�Dv��Dw!�Dw��Dx!�Dx��Dy!�Dy��Dz!�Dz��D{!�D{��D|!�D|��D}!�D}��D~!�D~��D!�D��D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�M�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D�D���D��D�P�DÐ�D���D��D�P�DĐ�D���D��D�P�DŐ�D���D��D�P�DƐ�D���D��D�P�Dǐ�D���D��D�P�DȐ�D���D��D�P�Dɐ�D���D��D�P�Dʐ�D���D��D�P�Dː�D���D��D�P�D̐�D���D��D�P�D͐�D���D��D�P�Dΐ�D���D��D�P�Dϐ�D���D��D�P�DА�D���D��D�P�Dѐ�D���D��D�P�DҐ�D���D��D�P�DӐ�D���D��D�P�DԐ�D���D��D�P�DՐ�D���D��D�P�D֐�D���D��D�P�Dא�D���D��D�P�Dؐ�D���D��D�P�Dِ�D���D��D�P�Dڐ�D���D��D�P�Dې�D���D��D�P�Dܐ�D���D��D�P�Dݐ�D���D��D�P�Dސ�D���D��D�P�Dߐ�D���D��D�P�D���D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�M�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D��)D�ʏ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�oA��A�oA��
Aȧ�A�^5A�1'AǼjA��;A�l�A�XA�VA��9A�-A�A�VA��A�/A�O�A��A��mA��A��\A��#A�M�A��A��DA�oA�`BA��9A���A��\A�ZA���A��PA�?}A���A���A�v�A��wA��A��A�Q�A�|�A��jA���A��DA�ZA�A��RA���A�`BA�1A��A�E�A�33A��
A�n�A�hsA��A��A���A���A�&�A�Q�A��/A��PA��7A�1'A���A�jA�K�A��A���A��TA���A��A�M�A��A���A���A��DA��A�A�K�A���A�|�A���A�(�A��RA�XA�`BA��RA�n�A���A��A�K�A��A��DAx�A}��A|��Ay��AvVAp��Am��Aml�Am�Al^5Ak7LAjffAh�jAa
=A^v�A\1'A[7LA\bA[C�AZVAX�`AWO�AU��AT�yAT{AS?}ARȴAQ�TAO�^AMC�AK��AJ��AF�\AC�7ABbNA@��A?x�A>��A<ĜA;p�A:��A:�A:VA:5?A9ƨA8��A8v�A8�A89XA6�A3�^A2^5A0�HA/oA-&�A,�A*�HA)�A)/A(�RA'ƨA&��A%��A%XA$��A#�A#t�A"�\A"A!33A�TA��A�/A�Az�AbNAbNAI�Ar�A�A%A�AAA��A��AbNA;dA{AĜA-AƨA�AȴAA�A
=A	\)A��AA�A7LA9XA|�AĜA��A�DA=qA�A�7A��AQ�A�A ff@�K�@�/@�b@���@��7@��@�ƨ@��!@��/@�l�@��@��D@@��@�&�@�@��@��@�O�@�!@噚@���@�C�@�-@�G�@� �@��@ܓu@ۅ@�o@�v�@�Ĝ@�v�@�G�@ӝ�@җ�@��@��;@�;d@�`B@�C�@�$�@ț�@��@�n�@���@���@Ə\@�n�@Ł@�+@��#@�bN@���@�=q@��@�bN@��
@�
=@�v�@�=q@���@�Z@��@�@��#@�A�@���@�dZ@�j@��@��F@�v�@�J@��@���@�K�@��
@��@���@�|�@��w@��@�;d@�~�@��#@��j@���@���@��j@�b@�dZ@�5?@��@���@���@���@��j@��@��@�;d@�^5@�p�@��@�bN@�  @��@���@�;d@��+@�M�@�{@��@�p�@�/@��@�bN@�b@��;@�ƨ@���@�\)@�"�@���@�$�@��T@��-@���@��h@�p�@�7L@���@��@��F@�l�@�o@���@��R@�-@��^@���@�O�@�7L@�7L@�V@���@�%@��@�b@���@��w@���@��@�@��@�ff@�=q@�@�@�p�@�hs@��@��@�7L@���@�z�@�1'@��@��m@���@�S�@�\)@�dZ@�\)@�+@�o@���@��@���@��+@�5?@�@���@��@���@��h@�hs@�`B@�X@�O�@�G�@�/@�%@��/@��u@�r�@�Z@�Q�@�I�@�(�@�1@��@��;@��w@��w@�|�@�S�@�;d@��@�ȴ@��+@�=q@��@�@���@���@���@��h@�p�@�G�@��@���@���@��/@���@�bN@�A�@��;@��@�\)@��@��@���@�~�@��@��T@���@��@�p�@�7L@�/@�V@��@�j@�1'@�b@���@���@��P@�t�@�S�@�C�@�"�@�o@�@�@�
=@�o@���@��H@���@��+@�ff@�=q@���@���@�hs@��@��@���@��@�Z@�9X@�b@��m@��
@�ƨ@��w@���@��@��@���@��+@�n�@�V@�5?@�{@��@��T@�@��h@�x�@�hs@�X@�%@���@���@�z�@� �@�;@�w@�@K�@~��@~ff@~E�@~5?@~@}�-@}p�@}�@|��@{��@{t�@{o@z��@z��@z�!@z~�@z�@y�#@y�^@y��@x��@x�9@x�u@x�@xA�@w|�@vv�@vV@vV@vV@v$�@u�@u�h@uV@t�@t�D@tj@tZ@s�F@s"�@so@so@so@r��@q��@q��@q��@qhs@q�@p��@p �@o|�@n��@n5?@m�T@m`B@l�D@k�m@kC�@k@j�H@j��@j��@j�\@j~�@jM�@i�#@i�^@ihs@i�@h��@h �@g�w@g|�@g+@g�@fȴ@fv�@fV@fff@f5?@e�-@d�j@dI�@d1@co@b�!@b^5@b=q@b-@b�@bJ@a��@`Ĝ@`�@`r�@`Q�@`b@_�@_��@_K�@^��@^��@]�@]p�@]V@\��@\(�@[�
@[dZ@[o@Z�H@Zn�@Y�^@Yhs@Y%@X�9@X��@XbN@XA�@W�;@W�P@W|�@W+@Vȴ@Vv�@VE�@U��@UO�@T��@Tz�@S��@SC�@R�!@R^5@Q��@Q��@Qhs@Q7L@P�`@P �@O�P@N��@Nff@NE�@M�h@L��@L�D@LI�@K�m@K"�@J��@JM�@J�@I�^@IX@H�`@H��@HbN@G�;@G��@G
=@F�@F��@Fv�@E�-@E/@D�@D�j@D��@D�D@DZ@C�m@C��@CS�@Co@B�@B�@B��@BJ@A��@Ahs@A�@@��@@��@@Q�@?��@?�@>ȴ@>ȴ@>�R@>v�@=@=`B@=O�@=/@=V@<�@<z�@;��@;ƨ@;��@;dZ@:�H@:��@:��@:n�@:J@9��@9�7@97L@8��@7�@7��@7K�@6�y@6��@6V@6E�@65?@6$�@6$�@5�@5�h@5?}@4�j@4I�@3��@3�F@3�@3@2�H@2��@2��@2^5@2M�@2-@1��@1�@1��@1X@17L@1�@1%@0��@0�`@0��@0�u@0r�@0bN@01'@0  @/�w@/;d@.�y@.ȴ@.�R@.ff@-��@-�h@-��@-p�@-?}@-/@-V@,�j@,�D@,9X@,1@+�m@+��@+o@*�H@*��@*��@*�!@*�!@*M�@)�#@)��@)x�@)hs@)hs@)X@)G�@)7L@)7L@)�@(��@(Q�@'�@'�P@'|�@'\)@'\)@'\)@'K�@';d@'�@&�@&��@&��@&V@&$�@%�@%��@%`B@%V@$��@$��@$z�@$j@$j@$Z@$I�@$9X@$(�@$1@#�
@#�
@#ƨ@#��@#��@#t�@#C�@#"�@#o@#o@"��@"~�@"^5@!�@!��@!X@!X@!X@!G�@!&�@ ��@ ��@ �u@ r�@ A�@ 1'@ b@��@��@l�@�@�R@E�@�-@�@`B@/@V@V@��@�/@�D@9X@1@��@33@33@��@n�@^5@-@�#@hs@G�@&�@�@��@�`@��@�9@Q�@ �@  @�P@l�@
=@��@E�@�-@�h@`B@`B@?}@V@�/@��@Z@(�@I�@j@Z@I�@9X@1@�
@ƨ@��@t�@"�@�@��@n�@-@�^@��@��@hs@X@&�@�9@�9@��@�@bN@A�@  @�;@�w@|�@;d@+@+@
=@ȴ@�+@v�@v�@v�@v�@v�@@��@�-@p�@�@�j@��@z�@I�@1@�m@ƨ@��@t�@t�@t�@33@
�@
��@
�!@
�\@
�\@
�\@
n�@
^5@
=q@
J@	��@	�^@	�7@	7L@��@Ĝ@�@r�@�@�u@bN@A�@ �@  @�;@��@�P@+@�y@ȴ@��@��@ff@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�oA��A�oA��
Aȧ�A�^5A�1'AǼjA��;A�l�A�XA�VA��9A�-A�A�VA��A�/A�O�A��A��mA��A��\A��#A�M�A��A��DA�oA�`BA��9A���A��\A�ZA���A��PA�?}A���A���A�v�A��wA��A��A�Q�A�|�A��jA���A��DA�ZA�A��RA���A�`BA�1A��A�E�A�33A��
A�n�A�hsA��A��A���A���A�&�A�Q�A��/A��PA��7A�1'A���A�jA�K�A��A���A��TA���A��A�M�A��A���A���A��DA��A�A�K�A���A�|�A���A�(�A��RA�XA�`BA��RA�n�A���A��A�K�A��A��DAx�A}��A|��Ay��AvVAp��Am��Aml�Am�Al^5Ak7LAjffAh�jAa
=A^v�A\1'A[7LA\bA[C�AZVAX�`AWO�AU��AT�yAT{AS?}ARȴAQ�TAO�^AMC�AK��AJ��AF�\AC�7ABbNA@��A?x�A>��A<ĜA;p�A:��A:�A:VA:5?A9ƨA8��A8v�A8�A89XA6�A3�^A2^5A0�HA/oA-&�A,�A*�HA)�A)/A(�RA'ƨA&��A%��A%XA$��A#�A#t�A"�\A"A!33A�TA��A�/A�Az�AbNAbNAI�Ar�A�A%A�AAA��A��AbNA;dA{AĜA-AƨA�AȴAA�A
=A	\)A��AA�A7LA9XA|�AĜA��A�DA=qA�A�7A��AQ�A�A ff@�K�@�/@�b@���@��7@��@�ƨ@��!@��/@�l�@��@��D@@��@�&�@�@��@��@�O�@�!@噚@���@�C�@�-@�G�@� �@��@ܓu@ۅ@�o@�v�@�Ĝ@�v�@�G�@ӝ�@җ�@��@��;@�;d@�`B@�C�@�$�@ț�@��@�n�@���@���@Ə\@�n�@Ł@�+@��#@�bN@���@�=q@��@�bN@��
@�
=@�v�@�=q@���@�Z@��@�@��#@�A�@���@�dZ@�j@��@��F@�v�@�J@��@���@�K�@��
@��@���@�|�@��w@��@�;d@�~�@��#@��j@���@���@��j@�b@�dZ@�5?@��@���@���@���@��j@��@��@�;d@�^5@�p�@��@�bN@�  @��@���@�;d@��+@�M�@�{@��@�p�@�/@��@�bN@�b@��;@�ƨ@���@�\)@�"�@���@�$�@��T@��-@���@��h@�p�@�7L@���@��@��F@�l�@�o@���@��R@�-@��^@���@�O�@�7L@�7L@�V@���@�%@��@�b@���@��w@���@��@�@��@�ff@�=q@�@�@�p�@�hs@��@��@�7L@���@�z�@�1'@��@��m@���@�S�@�\)@�dZ@�\)@�+@�o@���@��@���@��+@�5?@�@���@��@���@��h@�hs@�`B@�X@�O�@�G�@�/@�%@��/@��u@�r�@�Z@�Q�@�I�@�(�@�1@��@��;@��w@��w@�|�@�S�@�;d@��@�ȴ@��+@�=q@��@�@���@���@���@��h@�p�@�G�@��@���@���@��/@���@�bN@�A�@��;@��@�\)@��@��@���@�~�@��@��T@���@��@�p�@�7L@�/@�V@��@�j@�1'@�b@���@���@��P@�t�@�S�@�C�@�"�@�o@�@�@�
=@�o@���@��H@���@��+@�ff@�=q@���@���@�hs@��@��@���@��@�Z@�9X@�b@��m@��
@�ƨ@��w@���@��@��@���@��+@�n�@�V@�5?@�{@��@��T@�@��h@�x�@�hs@�X@�%@���@���@�z�@� �@�;@�w@�@K�@~��@~ff@~E�@~5?@~@}�-@}p�@}�@|��@{��@{t�@{o@z��@z��@z�!@z~�@z�@y�#@y�^@y��@x��@x�9@x�u@x�@xA�@w|�@vv�@vV@vV@vV@v$�@u�@u�h@uV@t�@t�D@tj@tZ@s�F@s"�@so@so@so@r��@q��@q��@q��@qhs@q�@p��@p �@o|�@n��@n5?@m�T@m`B@l�D@k�m@kC�@k@j�H@j��@j��@j�\@j~�@jM�@i�#@i�^@ihs@i�@h��@h �@g�w@g|�@g+@g�@fȴ@fv�@fV@fff@f5?@e�-@d�j@dI�@d1@co@b�!@b^5@b=q@b-@b�@bJ@a��@`Ĝ@`�@`r�@`Q�@`b@_�@_��@_K�@^��@^��@]�@]p�@]V@\��@\(�@[�
@[dZ@[o@Z�H@Zn�@Y�^@Yhs@Y%@X�9@X��@XbN@XA�@W�;@W�P@W|�@W+@Vȴ@Vv�@VE�@U��@UO�@T��@Tz�@S��@SC�@R�!@R^5@Q��@Q��@Qhs@Q7L@P�`@P �@O�P@N��@Nff@NE�@M�h@L��@L�D@LI�@K�m@K"�@J��@JM�@J�@I�^@IX@H�`@H��@HbN@G�;@G��@G
=@F�@F��@Fv�@E�-@E/@D�@D�j@D��@D�D@DZ@C�m@C��@CS�@Co@B�@B�@B��@BJ@A��@Ahs@A�@@��@@��@@Q�@?��@?�@>ȴ@>ȴ@>�R@>v�@=@=`B@=O�@=/@=V@<�@<z�@;��@;ƨ@;��@;dZ@:�H@:��@:��@:n�@:J@9��@9�7@97L@8��@7�@7��@7K�@6�y@6��@6V@6E�@65?@6$�@6$�@5�@5�h@5?}@4�j@4I�@3��@3�F@3�@3@2�H@2��@2��@2^5@2M�@2-@1��@1�@1��@1X@17L@1�@1%@0��@0�`@0��@0�u@0r�@0bN@01'@0  @/�w@/;d@.�y@.ȴ@.�R@.ff@-��@-�h@-��@-p�@-?}@-/@-V@,�j@,�D@,9X@,1@+�m@+��@+o@*�H@*��@*��@*�!@*�!@*M�@)�#@)��@)x�@)hs@)hs@)X@)G�@)7L@)7L@)�@(��@(Q�@'�@'�P@'|�@'\)@'\)@'\)@'K�@';d@'�@&�@&��@&��@&V@&$�@%�@%��@%`B@%V@$��@$��@$z�@$j@$j@$Z@$I�@$9X@$(�@$1@#�
@#�
@#ƨ@#��@#��@#t�@#C�@#"�@#o@#o@"��@"~�@"^5@!�@!��@!X@!X@!X@!G�@!&�@ ��@ ��@ �u@ r�@ A�@ 1'@ b@��@��@l�@�@�R@E�@�-@�@`B@/@V@V@��@�/@�D@9X@1@��@33@33@��@n�@^5@-@�#@hs@G�@&�@�@��@�`@��@�9@Q�@ �@  @�P@l�@
=@��@E�@�-@�h@`B@`B@?}@V@�/@��@Z@(�@I�@j@Z@I�@9X@1@�
@ƨ@��@t�@"�@�@��@n�@-@�^@��@��@hs@X@&�@�9@�9@��@�@bN@A�@  @�;@�w@|�@;d@+@+@
=@ȴ@�+@v�@v�@v�@v�@v�@@��@�-@p�@�@�j@��@z�@I�@1@�m@ƨ@��@t�@t�@t�@33@
�@
��@
�!@
�\@
�\@
�\@
n�@
^5@
=q@
J@	��@	�^@	�7@	7L@��@Ĝ@�@r�@�@�u@bN@A�@ �@  @�;@��@�P@+@�y@ȴ@��@��@ff@ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�B
�B
�B
�!B
�'B
�3B
�-B
�?B
�jB
ȴB
��B
�/BBbBhB8RB�B�BB�B�B�BhB+B0!B33B:^B:^B>wB@�B@�BB�B?}B:^B.B�BuB&�BM�Bz�B�VB�hB�bB�VB�Bt�Bs�Br�Bp�Bl�Bl�Br�Bo�Bm�BcTB^5BP�BF�B2-B&�B%�B/B.B�B  B��B��B��B  B�B�B�B$�B>wB:^B5?B33B+B#�B�BbB+B��B�B�BB��B�}B��B�?B��By�B[#BB�B/B
��B
�FB
��B
u�B
jB
`BB
M�B
>wB
2-B
�B	�B	B	��B	��B	��B	�VB	}�B	o�B	YB	�B��B�B�B	1'B	A�B	@�B	9XB	33B	)�B	&�B	 �B	�B	{B	PB	B��B�B�`B��B��B�jB�LB�?B�dB�3B�B�B��B��B��B��B��B��B��B�B��B�JB�B}�B{�Bu�Bs�Bo�Bn�Bl�BjBjBiyBiyBhsBffBcTB`BB\)B]/BaHB`BBbNB`BB[#BP�BP�BP�BO�BL�BH�BI�BJ�BJ�BG�BD�BE�BA�B<jB=qB=qB>wB@�B9XB/B-B0!B'�B(�B-B'�B&�B&�B%�B$�B$�B$�B$�B$�B#�B"�B#�B#�B!�B#�B!�B"�B �B �B!�B �B!�B �B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B"�B"�B$�B$�B$�B&�B(�B'�B,B,B/B6FB9XB:^B?}BB�B>wB<jB<jB<jB<jB<jB=qB>wB?}BA�BA�BC�BE�BE�BF�BG�BI�BM�BO�B[#BiyBjBiyBk�Bk�BhsBn�Bt�Bw�Bw�B{�B�B�%B�7B�PB�\B�bB�uB�{B��B��B��B��B��B�B�B�B�B�?B�RB�dB�jB�wB�wBÖBƨB��B��B�B�;B�NB�ZB�`B�yB�B�B��B��B��B��B��B	B	B	1B	PB	\B	hB	hB	oB	uB	�B	�B	�B	�B	�B	#�B	#�B	%�B	(�B	,B	1'B	6FB	;dB	=qB	A�B	C�B	D�B	E�B	D�B	E�B	F�B	G�B	G�B	J�B	K�B	N�B	O�B	Q�B	W
B	[#B	]/B	_;B	_;B	_;B	_;B	_;B	`BB	bNB	dZB	e`B	ffB	hsB	jB	l�B	m�B	m�B	m�B	n�B	o�B	q�B	s�B	t�B	u�B	u�B	v�B	z�B	|�B	}�B	~�B	~�B	� B	�B	�B	�B	�B	�1B	�7B	�DB	�DB	�PB	�VB	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�9B	�?B	�FB	�LB	�LB	�^B	�jB	�wB	�wB	��B	B	ĜB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�BB	�BB	�HB	�TB	�ZB	�ZB	�`B	�fB	�fB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
B
B
%B
+B
+B
1B
+B
1B
1B
1B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B
DB
DB
JB
JB
JB
PB
PB
PB
PB
PB
VB
bB
hB
hB
hB
hB
oB
oB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
+B
+B
+B
+B
,B
,B
,B
,B
,B
,B
-B
-B
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
1'B
1'B
1'B
1'B
1'B
2-B
33B
33B
33B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
7LB
8RB
8RB
7LB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
:^B
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
>wB
?}B
?}B
?}B
?}B
?}B
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
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
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
F�B
F�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
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
N�B
O�B
O�B
O�B
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
Q�B
R�B
R�B
R�B
R�B
R�B
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
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
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
gmB
gmB
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
jB
jB
jB
jB
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
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
o�B
o�B
o�B
o�B
p�B
q�B
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
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
t�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
��B
��B
��B
��B
��B
��B
��B
�B
�PB
ȚB
��B
��B�B.B4B8B��B�B�B�oB�B4B*�B/�B2�B:*B:*B>BB@OB@OBB[B?HB:*B-�B~B@B&�BM�Bz�B�"B�4B�.B�"B��Bt�Bs�Br|BpoBlWBlqBr|BoiBm]Bc B^BP�BFtB1�B&�B%�B.�B-�BMB��B��B��B��B��BMB_BkB$�B>BB:*B5B2�B*�B#�B~B.B�B��B�vB�BҽB�HB�OB�B��By�BZ�BB[B.�B
��B
�B
�SB
u�B
jKB
`B
M�B
>BB
1�B
YB	�vB	�[B	��B	�qB	�SB	�"B	}�B	oOB	X�B	WB��B�|B�|B	0�B	AUB	@OB	9$B	2�B	)�B	&�B	 �B	eB	FB	B	 �B��B�KB�,B˒B�OB�6B�B�B�0B��B��B��B��B��B��B��B��B��B��B��B��B��B��B}�B{�Bu�Bs�BoiBnIBlWBjKBj0BiDBiDBh$Bf2Bc B_�B[�B\�B`�B`Ba�B_�BZ�BP�BP�BP�BO�BL�BHfBI�BJ�BJ�BG_BDMBEmBA;B<6B=<B=<B>BB@OB9	B.�B,�B/�B'�B(�B,�B'�B&�B&�B%�B$�B$�B$�B$�B$�B#�B"�B#�B#�B!�B#�B!�B"�B �B vB!�B vB!|B vB!�BpB�BjBjBdB~BdBWB_BYB9B9BSB_B9BeBkBQBeBeB~BpB vB"�B"�B$�B$�B$�B&�B(�B'�B+�B+�B.�B5�B9	B:B?.BBAB>BB<B<B<6B<B<B="B>(B?.BA;BA;BCaBEmBESBFYBG_BI�BM�BO�BZ�Bi*BjKBi*Bk6Bk6Bh$BncBtnBw�Bw�B{�B��B��B�B�B�B�B�&B�FB�9B�_B�eB�jB��B��B��B��B��B�B�B�B�B�(B�(B�GB�tBϫBөB��B��B��B�&B�,B�*B�]B�hB�zB��B��B��B��B	 �B	�B	�B	B	B	B	B	:B	@B	2B	SB	QB	]B	�B	#�B	#�B	%�B	(�B	+�B	0�B	5�B	;B	="B	A;B	CaB	DgB	ESB	DgB	ESB	FtB	G_B	G_B	JrB	KxB	N�B	O�B	Q�B	V�B	Z�B	\�B	^�B	_B	^�B	^�B	_B	_�B	a�B	dB	eB	fB	h>B	j0B	l=B	m]B	m]B	m]B	nIB	oOB	qvB	shB	tnB	utB	utB	vzB	z�B	|�B	}�B	~�B	~�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�.B	�B	�:B	�&B	�?B	�EB	�KB	�QB	��B	��B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�BB	�BB	�UB	�[B	�gB	�YB	�_B	�fB	ɆB	�rB	̈́B	ϑB	ѷB	ѝB	ңB	ҽB	ңB	өB	��B	յB	յB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	� B	�&B	�&B	�B	�B	�B	�>B	�*B	�*B	�*B	�DB	�*B	�=B	�]B	�CB	�cB	�cB	�OB	�OB	�UB	�UB	�vB	�vB	�|B	�|B	�aB	�aB	�hB	�nB	�nB	�B	�tB	�zB	�zB	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	B
�B

	B

	B
	�B

�B
B
�B
B
�B
B
B
B
B
B
"B
B
B
B
B
B
:B
 B
&B
&B
&B
,B
,B
FB
,B
MB
2B
MB
2B
MB
2B
2B
SB
9B
SB
9B
?B
YB
EB
_B
eB
KB
QB
QB
WB
]B
xB
]B
dB
~B
jB
dB
�B
�B
pB
 vB
!|B
!|B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
%�B
%�B
%�B
&�B
'�B
'�B
'�B
(�B
(�B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
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
0�B
0�B
0�B
0�B
0�B
1�B
2�B
2�B
2�B
3�B
4B
3�B
3�B
4�B
4�B
4�B
5�B
5�B
7B
8B
8B
6�B
8B
9	B
9$B
9	B
9	B
9$B
9$B
:B
;B
;B
;B
;B
;0B
;B
;B
;B
<B
<6B
<B
<B
="B
="B
>BB
>(B
>(B
>(B
?.B
?HB
?.B
?.B
?.B
@OB
@4B
@OB
@4B
A;B
AUB
BAB
BAB
B[B
BAB
BAB
BAB
CaB
CGB
CGB
CGB
CGB
DMB
DgB
DMB
DMB
DgB
DMB
DgB
ESB
EmB
EmB
ESB
ESB
ESB
ESB
FYB
FYB
FYB
FtB
FYB
G_B
G_B
H�B
H�B
H�B
IlB
IlB
JrB
JrB
K�B
K�B
K�B
J�B
JrB
JrB
KxB
KxB
KxB
KxB
L~B
L~B
L~B
L~B
L�B
L~B
L�B
L~B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
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
Q�B
R�B
R�B
R�B
R�B
R�B
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
\�B
\�B
]�B
]�B
]�B
]�B
^B
]�B
^�B
^�B
_�B
_�B
`B
_�B
`B
_�B
`B
_�B
_�B
`�B
`�B
`�B
aB
aB
bB
a�B
a�B
bB
bB
bB
bB
bB
c B
cB
cB
dB
eB
e,B
fB
f2B
fB
fB
fB
g8B
g8B
gB
g8B
g8B
g8B
gB
h>B
h>B
h$B
h$B
h$B
h>B
h>B
i*B
i*B
iDB
i*B
iDB
i*B
j0B
jKB
j0B
jKB
k6B
l=B
lWB
l=B
l=B
m]B
mCB
mCB
mCB
m]B
m]B
m]B
m]B
mCB
ncB
nIB
oOB
oiB
oOB
oOB
oiB
oOB
oOB
oiB
oiB
poB
q[B
q[B
q[B
q[B
qvB
q[B
q[B
r|B
raB
r|B
raB
raB
raB
r|B
r|B
raB
raB
raB
r|B
raB
r|B
s�B
s�B
shB
s�B
shB
shB
shB
s�B
shB
s�B
shB
s�B
shB
t�B
tn1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.53(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905260036522019052600365220190526003652201905280045542019052800455420190528004554JA  ARFMdecpA19c                                                                20190606181442  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190606091808  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190606091809  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190606091810  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190606091810  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190606091810  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190606091810  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190606091810  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190606091811  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190606091811                      G�O�G�O�G�O�                JA  ARUP                                                                        20190606101514                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190520153316  CV  JULD            G�O�G�O�F���                JM  ARCAJMQC2.0                                                                 20190525153652  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190525153652  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190527154554  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120031518                      G�O�G�O�G�O�                