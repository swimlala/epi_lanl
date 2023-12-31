CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-11-26T12:37:07Z creation;2018-11-26T12:37:10Z conversion to V3.1;2019-12-23T06:11:19Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181126123707  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               jA   JA  I2_0675_106                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ؓ�Ϳ��1   @ؓ���-�@7(����cHj~��#1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ D�|�D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�3D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�6f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@���Az�A(z�AHz�Aj{A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B��)B�\B�\B�\C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�P�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D !�D ��D!�D��D!�D��D!�D��D�D��D!�D��D!�D��D!�D��D!�D��D	!�D	��D
!�D
��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D!�D��D !�D ��D!!�D!��D"!�D"��D#!�D#��D$!�D$��D%!�D%��D&!�D&��D'!�D'��D(!�D(��D)!�D)��D*!�D*��D+!�D+��D,!�D,��D-!�D-��D.!�D.��D/!�D/��D0!�D0��D1!�D1��D2!�D2��D3!�D3��D4!�D4��D5!�D5��D6!�D6��D7!�D7��D8!�D8��D9!�D9��D:!�D:��D;!�D;��D<!�D<��D=!�D=��D>!�D>��D?!�D?��D@!�D@��DA!�DA��DB!�DB��DC!�DC��DD!�DD��DE!�DE��DF!�DF��DG!�DG��DH!�DH��DI!�DI��DJ!�DJ��DK!�DK��DL!�DL��DM!�DM��DN!�DN��DO!�DO��DP!�DP��DQ!�DQ��DR!�DR��DS!�DS��DT!�DT��DU!�DU��DV!�DV��DW!�DW��DX!�DX��DY!�DY��DZ!�DZ��D[!�D[��D\!�D\��D]!�D]��D^!�D^��D_!�D_��D`!�D`��Da!�Da��Db!�Db��Dc!�Dc��Dd!�Dd��De!�De��Df!�Df��Dg!�Dg��Dh!�Dh��Di!�Di��Dj!�Dj��Dk!�Dk��Dl!�Dl��Dm!�Dm��Dn!�Dn��Do!�Do��Dp!�Dp��Dq!�Dq��Dr!�Dr��Ds!�Ds��Dt!�Dt��Du!�Du��Dv!�Dv��Dw!�Dw��Dx!�Dx��Dy!�Dy��Dz!�Dz��D{!�D{��D|!�D|��D}!�D}��D~!�D~��D!�D��D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D��)D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�M�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D��D�P�D�D���D��D�P�DÐ�D���D��D�P�DĐ�D���D��D�P�DŐ�D���D��D�P�DƐ�D���D��D�P�Dǐ�D���D��D�P�DȐ�D���D��D�P�Dɐ�D���D��D�P�Dʍ�D���D��D�P�Dː�D���D��D�P�D̐�D���D��D�P�D͐�D���D��D�P�Dΐ�D���D��D�P�Dϐ�D���D��D�P�DА�D���D��D�P�Dѐ�D���D��D�P�DҐ�D���D��D�P�DӐ�D���D��D�P�DԐ�D���D��D�M�DՐ�D���D��D�P�D֐�D���D��D�P�Dא�D���D��D�P�Dؐ�D���D��D�P�Dِ�D���D��D�P�Dڐ�D���D��D�P�Dې�D���D�)D�P�Dܐ�D���D��D�P�Dݐ�D���D��D�P�Dސ�D���D��D�P�Dߐ�D���D��D�P�D���D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D��D���D��D�P�D���D���D��D�P�D���D���D��D�P�D���D���D�)D�G\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ZA�`BA�`BA�\)A�O�A�C�A�A�A�oA���A���A˶FAˬA˧�Aˣ�A˥�A˥�A˩�AˮA˰!AˮAˡ�A˗�A˙�Aˡ�A˟�A˗�AˋDA˅A˅A˅Aˉ7Aˉ7A�~�A��A�dZA�"�A��A�|�A�ĜA��HA���A�5?A�ffA�  A���A��;A��A�"�A��+A��
A���A��A��A�5?A�A�M�A�A���A��#A�5?A�C�A�VA��A�hsA�-A��A�ffA���A�(�A�t�A���A��\A�`BA�%A�VA���A�A�A���A���A�/A�E�A��yA���A�(�A�^5A�l�A��9A��9A���A��A�=qA�JA��A�A��#A���A���A��\A��A��A��9A�K�A���A��uA���A�33A��;A�ȴA��wAA}�Az�Ay�Ax�`Av~�AtM�AsO�Ar��Aq�Ap(�AnbAk/Aip�Ag&�AeG�Ab�A`Q�A\�RAZE�AXbNAV$�AT�`AT~�AQ�AP~�AN��AM\)ALr�AK��AKO�AH��AGC�AF��AF$�AD��ACXABĜAB9XAA��A@�HA@�A?ƨA>ZA=��A<�jA;�A;VA:�uA97LA8z�A7�A6�A5\)A4�RA4�A3�A2�jA0��A-�^A,JA*ȴA)oA'��A&�HA&M�A$�A#��A"�HA"�uA"1A!G�A �jA 9XA�yAA�A��AhsA+A^5A`BA7LA%A��AbA��A�hA��A�A��A�!A�
A��An�A=qA�#A��A"�A
1'A	XA��AVA��A��A�A
=AjA��AdZA��AE�A��@��@�/@�"�@���@���@�z�@��
@�M�@�r�@�=q@�@�t�@�D@�x�@�@�x�@�@�?}@��u@�Z@��m@���@ޏ\@�=q@ݩ�@�Z@��@��@և+@�O�@ӍP@ҏ\@�V@�x�@�9X@�ƨ@υ@���@�{@�O�@���@̋D@��@ʰ!@��@ɩ�@�&�@�Ĝ@�z�@�I�@�~�@Ĭ@��@�"�@�ff@��`@�j@�b@��@�S�@��@��y@���@�V@�7L@�I�@�b@��w@�l�@�o@��\@��@��7@�G�@��/@��w@��@��\@�E�@�J@���@�ƨ@���@�V@�@��7@�Z@�  @�;d@���@�ff@�V@�E�@���@��7@�x�@�(�@�S�@�^5@��7@�Q�@�b@�1@�ƨ@��H@��@��9@�Q�@��@��F@��P@�+@�@��H@��!@��\@�n�@�J@��7@��@�%@���@���@��u@�r�@�b@���@�t�@�dZ@�
=@�ȴ@��\@�M�@�5?@�{@�{@���@��#@��h@�V@��u@�Q�@�1@��F@�C�@�;d@��@���@��!@�{@��h@��@��@��@���@�j@���@�C�@�33@�
=@�@�O�@���@�r�@�I�@�(�@�  @�  @���@��@�"�@��!@�ff@���@���@��+@���@���@��^@�p�@���@���@��h@��h@�p�@�X@�?}@���@���@�b@��@�33@�
=@��\@���@��!@��R@��R@�ȴ@��y@��@�
=@�"�@�S�@�33@��y@���@�n�@�=q@�@���@�hs@��@��D@�9X@���@��m@��;@���@��@�
=@��R@�~�@�n�@�V@�J@���@���@�`B@�V@���@��/@���@�Ĝ@��@�A�@��@��@��
@��w@�ƨ@�ƨ@�\)@�
=@��@��y@���@�5?@�{@�@��@��h@�p�@�?}@���@��j@�r�@�A�@�1'@�1@��
@��@��P@�\)@�+@�o@��H@���@�=q@��#@���@��^@���@��h@��@�p�@��@���@���@�r�@�1@�@|�@~��@~v�@}O�@|�j@{��@{�@{"�@z��@z-@yx�@x��@xQ�@x  @w�@w�P@v�y@vff@u��@u��@u`B@u?}@uV@t�/@t�/@t�/@t��@t��@t��@t��@t�@sS�@so@r�H@r�!@r~�@r^5@q��@q�7@q7L@p�`@p�9@pbN@o�;@o��@o��@o�P@o\)@o+@n�y@n{@mO�@l�@l�j@lj@l(�@l�@k�F@kdZ@k@j�H@j��@jn�@i�@i��@i�^@i�^@i�^@i�7@ix�@i�@h�`@h�`@h��@hĜ@hbN@hA�@g�@g|�@f��@f��@fE�@ep�@eV@d�@d�@d��@d�@d9X@c�F@c33@c@b�H@b��@bM�@a�#@a�7@a7L@a&�@a�@`��@`�9@`�@_�@_��@_�w@_��@_��@_|�@_l�@_;d@^�@^V@]�@]�-@]O�@\�/@\��@\��@\��@\�D@\�D@\j@\9X@\�@[�F@[dZ@[C�@[33@[@Z�H@Z��@Y��@Y�^@YX@X��@XQ�@X �@W��@W+@V��@V�@V5?@V@U��@Up�@UV@T��@Tj@T1@S��@SC�@R�H@R�!@Rn�@RM�@R�@Q��@QG�@P�9@O�w@O+@Nȴ@N�+@M�@M��@M?}@L�j@Lz�@K�
@K��@K"�@K@J�H@J^5@I�#@Ix�@I�@H�9@HQ�@G��@G�P@G|�@G|�@F�y@F�+@Fv�@Fff@E�T@D��@D��@D(�@C�
@C��@Ct�@Ct�@CS�@C"�@Co@C@B�!@B~�@BM�@A�#@A��@Ahs@AG�@@Ĝ@@��@@�@@1'@?�@?�@?\)@>�@>v�@>V@>$�@>@=p�@=/@=V@<�@<Z@<�@;dZ@:�H@:�!@:�\@:^5@9�@9hs@97L@8�`@8�@8bN@81'@7�;@7�@7�P@7\)@7K�@7+@7+@7+@7�@7�@6�@65?@6@5��@5`B@5?}@4��@4�j@4j@4(�@3�F@3t�@3C�@333@3o@3o@2��@2�\@2n�@2^5@2�@1��@1x�@1G�@1�@0��@0��@0r�@0A�@/�@/��@/�w@/��@/l�@/+@.v�@.{@.@-��@-��@-�@-�@-`B@,�@,�D@,I�@,9X@,(�@,�@+��@+�F@+�@+dZ@+33@*�@*�\@*-@)�@)�@)�#@)�#@)�7@)X@)%@(��@(Ĝ@(Ĝ@(��@(r�@(Q�@( �@( �@'�w@'|�@'l�@'K�@'+@'�@&��@&��@&�y@&�@&��@&5?@&{@&@%�@%@%@%��@%p�@%/@$�/@$�@$z�@$9X@#��@#�m@#ƨ@#��@#��@#��@#��@#�@#o@"��@"��@"=q@"�@!��@!X@!7L@ �`@ ��@ ��@ �9@ ��@ ��@ �u@ r�@ b@�@K�@+@�@ȴ@v�@ff@E�@{@�@�@��@�h@O�@/@�j@�@1@�m@��@t�@S�@o@@�\@��@�@�@�^@hs@&�@&�@�@�`@�u@  @|�@;d@�y@ȴ@�+@{@�-@�h@p�@/@��@�j@�D@�D@j@z�@9X@�F@dZ@33@@@�@�H@��@�!@M�@�@x�@X@��@�9@�@�@�@r�@�w@|�@;d@��@ȴ@�R@��@�+@E�@5?@$�@�@��@@�-@�h@�@�@p�@O�@?}@?}@/@V@�@�@�/@�j@j@�
@ƨ@ƨ@�F@t�@dZ@C�@33@o@
�H@
��@
��@
�H@
��@
M�@
=q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ZA�`BA�`BA�\)A�O�A�C�A�A�A�oA���A���A˶FAˬA˧�Aˣ�A˥�A˥�A˩�AˮA˰!AˮAˡ�A˗�A˙�Aˡ�A˟�A˗�AˋDA˅A˅A˅Aˉ7Aˉ7A�~�A��A�dZA�"�A��A�|�A�ĜA��HA���A�5?A�ffA�  A���A��;A��A�"�A��+A��
A���A��A��A�5?A�A�M�A�A���A��#A�5?A�C�A�VA��A�hsA�-A��A�ffA���A�(�A�t�A���A��\A�`BA�%A�VA���A�A�A���A���A�/A�E�A��yA���A�(�A�^5A�l�A��9A��9A���A��A�=qA�JA��A�A��#A���A���A��\A��A��A��9A�K�A���A��uA���A�33A��;A�ȴA��wAA}�Az�Ay�Ax�`Av~�AtM�AsO�Ar��Aq�Ap(�AnbAk/Aip�Ag&�AeG�Ab�A`Q�A\�RAZE�AXbNAV$�AT�`AT~�AQ�AP~�AN��AM\)ALr�AK��AKO�AH��AGC�AF��AF$�AD��ACXABĜAB9XAA��A@�HA@�A?ƨA>ZA=��A<�jA;�A;VA:�uA97LA8z�A7�A6�A5\)A4�RA4�A3�A2�jA0��A-�^A,JA*ȴA)oA'��A&�HA&M�A$�A#��A"�HA"�uA"1A!G�A �jA 9XA�yAA�A��AhsA+A^5A`BA7LA%A��AbA��A�hA��A�A��A�!A�
A��An�A=qA�#A��A"�A
1'A	XA��AVA��A��A�A
=AjA��AdZA��AE�A��@��@�/@�"�@���@���@�z�@��
@�M�@�r�@�=q@�@�t�@�D@�x�@�@�x�@�@�?}@��u@�Z@��m@���@ޏ\@�=q@ݩ�@�Z@��@��@և+@�O�@ӍP@ҏ\@�V@�x�@�9X@�ƨ@υ@���@�{@�O�@���@̋D@��@ʰ!@��@ɩ�@�&�@�Ĝ@�z�@�I�@�~�@Ĭ@��@�"�@�ff@��`@�j@�b@��@�S�@��@��y@���@�V@�7L@�I�@�b@��w@�l�@�o@��\@��@��7@�G�@��/@��w@��@��\@�E�@�J@���@�ƨ@���@�V@�@��7@�Z@�  @�;d@���@�ff@�V@�E�@���@��7@�x�@�(�@�S�@�^5@��7@�Q�@�b@�1@�ƨ@��H@��@��9@�Q�@��@��F@��P@�+@�@��H@��!@��\@�n�@�J@��7@��@�%@���@���@��u@�r�@�b@���@�t�@�dZ@�
=@�ȴ@��\@�M�@�5?@�{@�{@���@��#@��h@�V@��u@�Q�@�1@��F@�C�@�;d@��@���@��!@�{@��h@��@��@��@���@�j@���@�C�@�33@�
=@�@�O�@���@�r�@�I�@�(�@�  @�  @���@��@�"�@��!@�ff@���@���@��+@���@���@��^@�p�@���@���@��h@��h@�p�@�X@�?}@���@���@�b@��@�33@�
=@��\@���@��!@��R@��R@�ȴ@��y@��@�
=@�"�@�S�@�33@��y@���@�n�@�=q@�@���@�hs@��@��D@�9X@���@��m@��;@���@��@�
=@��R@�~�@�n�@�V@�J@���@���@�`B@�V@���@��/@���@�Ĝ@��@�A�@��@��@��
@��w@�ƨ@�ƨ@�\)@�
=@��@��y@���@�5?@�{@�@��@��h@�p�@�?}@���@��j@�r�@�A�@�1'@�1@��
@��@��P@�\)@�+@�o@��H@���@�=q@��#@���@��^@���@��h@��@�p�@��@���@���@�r�@�1@�@|�@~��@~v�@}O�@|�j@{��@{�@{"�@z��@z-@yx�@x��@xQ�@x  @w�@w�P@v�y@vff@u��@u��@u`B@u?}@uV@t�/@t�/@t�/@t��@t��@t��@t��@t�@sS�@so@r�H@r�!@r~�@r^5@q��@q�7@q7L@p�`@p�9@pbN@o�;@o��@o��@o�P@o\)@o+@n�y@n{@mO�@l�@l�j@lj@l(�@l�@k�F@kdZ@k@j�H@j��@jn�@i�@i��@i�^@i�^@i�^@i�7@ix�@i�@h�`@h�`@h��@hĜ@hbN@hA�@g�@g|�@f��@f��@fE�@ep�@eV@d�@d�@d��@d�@d9X@c�F@c33@c@b�H@b��@bM�@a�#@a�7@a7L@a&�@a�@`��@`�9@`�@_�@_��@_�w@_��@_��@_|�@_l�@_;d@^�@^V@]�@]�-@]O�@\�/@\��@\��@\��@\�D@\�D@\j@\9X@\�@[�F@[dZ@[C�@[33@[@Z�H@Z��@Y��@Y�^@YX@X��@XQ�@X �@W��@W+@V��@V�@V5?@V@U��@Up�@UV@T��@Tj@T1@S��@SC�@R�H@R�!@Rn�@RM�@R�@Q��@QG�@P�9@O�w@O+@Nȴ@N�+@M�@M��@M?}@L�j@Lz�@K�
@K��@K"�@K@J�H@J^5@I�#@Ix�@I�@H�9@HQ�@G��@G�P@G|�@G|�@F�y@F�+@Fv�@Fff@E�T@D��@D��@D(�@C�
@C��@Ct�@Ct�@CS�@C"�@Co@C@B�!@B~�@BM�@A�#@A��@Ahs@AG�@@Ĝ@@��@@�@@1'@?�@?�@?\)@>�@>v�@>V@>$�@>@=p�@=/@=V@<�@<Z@<�@;dZ@:�H@:�!@:�\@:^5@9�@9hs@97L@8�`@8�@8bN@81'@7�;@7�@7�P@7\)@7K�@7+@7+@7+@7�@7�@6�@65?@6@5��@5`B@5?}@4��@4�j@4j@4(�@3�F@3t�@3C�@333@3o@3o@2��@2�\@2n�@2^5@2�@1��@1x�@1G�@1�@0��@0��@0r�@0A�@/�@/��@/�w@/��@/l�@/+@.v�@.{@.@-��@-��@-�@-�@-`B@,�@,�D@,I�@,9X@,(�@,�@+��@+�F@+�@+dZ@+33@*�@*�\@*-@)�@)�@)�#@)�#@)�7@)X@)%@(��@(Ĝ@(Ĝ@(��@(r�@(Q�@( �@( �@'�w@'|�@'l�@'K�@'+@'�@&��@&��@&�y@&�@&��@&5?@&{@&@%�@%@%@%��@%p�@%/@$�/@$�@$z�@$9X@#��@#�m@#ƨ@#��@#��@#��@#��@#�@#o@"��@"��@"=q@"�@!��@!X@!7L@ �`@ ��@ ��@ �9@ ��@ ��@ �u@ r�@ b@�@K�@+@�@ȴ@v�@ff@E�@{@�@�@��@�h@O�@/@�j@�@1@�m@��@t�@S�@o@@�\@��@�@�@�^@hs@&�@&�@�@�`@�u@  @|�@;d@�y@ȴ@�+@{@�-@�h@p�@/@��@�j@�D@�D@j@z�@9X@�F@dZ@33@@@�@�H@��@�!@M�@�@x�@X@��@�9@�@�@�@r�@�w@|�@;d@��@ȴ@�R@��@�+@E�@5?@$�@�@��@@�-@�h@�@�@p�@O�@?}@?}@/@V@�@�@�/@�j@j@�
@ƨ@ƨ@�F@t�@dZ@C�@33@o@
�H@
��@
��@
�H@
��@
M�@
=q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��BB�B�BF�BVB\)Bm�Bq�B{�B�+B�JB�\B�PB�PB�DB�VB�hB��B��B��B��B��B��B��B��B��B��B��B��B�DB�+B�Bv�BffBaHB\)BT�BM�BH�BD�B=qB9XB6FB33B0!B)�B'�B!�B�B�BDB��B�B�;B��B�XB��B�+Bk�Be`B[#BP�BO�BM�BE�B49B.B,B)�B$�B+B
�sB
�/B
�/B
ǮB
ǮB
�B
�=B
w�B
hsB
XB
C�B
8RB
49B
$�B
{B
JB
1B
B	��B	�TB	��B	��B	�?B	��B	�uB	�B	hsB	W
B	I�B	<jB	1'B	,B	�B	hB	B	B��B��B��B�sB�B�B��B��B��BɺBŢB��B�^B�XB�LB�'B�-B�'B�-B�B�B��B�B�B�B��B��B��B��B��B�hB�+B|�Bv�Bs�Bu�Bx�Bw�Bt�Bp�Bl�BjBiyBffBdZBbNB_;B]/B[#BZBYBW
BVBP�BN�BN�BM�BK�BH�BF�BC�B@�B?}B<jB<jB9XB8RB6FB5?B2-B1'B0!B0!B,B+B)�B(�B&�B$�B!�B �B!�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B%�B%�B&�B%�B(�B)�B)�B+B,B,B+B,B,B0!B0!B1'B2-B49B49B49B6FB7LB7LB7LB8RB8RB9XB9XB9XB:^B<jB=qB=qB?}B?}B@�B?}BE�BG�BI�BJ�BL�BO�BP�BQ�BR�BS�BS�BT�BT�BVBZB\)B]/B^5B^5B`BBaHBcTBcTBdZBdZBiyBjBk�Bl�Bl�Br�Bw�By�B{�B|�B~�B�B�B�1B�=B�=B�=B�=B�VB�VB�VB��B��B��B��B��B��B��B�B�'B�RBÖBƨB��B��B�B�B�)B�5B�HB�ZB�`B�B�B��B��B��B��B	  B	%B	JB	oB	{B	�B	�B	�B	!�B	"�B	$�B	%�B	%�B	&�B	'�B	+B	0!B	1'B	33B	7LB	9XB	>wB	>wB	?}B	@�B	B�B	F�B	K�B	O�B	P�B	Q�B	R�B	R�B	S�B	VB	VB	VB	ZB	[#B	`BB	cTB	e`B	iyB	l�B	s�B	u�B	u�B	u�B	w�B	z�B	�B	�%B	�7B	�=B	�=B	�JB	�bB	�oB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�9B	�FB	�XB	�^B	�dB	�dB	�dB	�wB	B	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�/B	�;B	�;B	�;B	�BB	�HB	�NB	�TB	�`B	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
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
+B
+B
+B
1B
	7B

=B

=B

=B
DB
DB
JB
JB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
hB
oB
oB
uB
uB
{B
uB
{B
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
$�B
$�B
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
)�B
)�B
)�B
+B
,B
,B
,B
-B
.B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
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
49B
49B
5?B
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
?}B
?}B
@�B
@�B
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
F�B
G�B
G�B
G�B
H�B
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
L�B
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
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
R�B
R�B
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
T�B
T�B
T�B
T�B
T�B
VB
VB
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
XB
XB
YB
YB
YB
YB
YB
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
]/B
]/B
]/B
]/B
^5B
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
dZB
dZB
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
gmB
gmB
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
m�B
m�B
m�B
m�B
m�B
n�B
o�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�cB�]B�}B�wB�cB�cB�}B�iB�cB�B�B�iB�oB�vB�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�BMB�BFtBU�B[�BmwBq�B{�B��B�B�(B�B�B�B�"B�4B�xB��B��B��B��B��B��B��B�xB�kB�_B�SB�B��B��Bv�Bf2BaB[�BT�BM�BH�BDgB=<B9$B6B2�B/�B)�B'�B!�BxBSBB��B�WB�BΥB�$B��B��BkQBe,BZ�BP�BO�BM�BEmB4B-�B+�B)�B$�B�B
�>B
��B
��B
�zB
�zB
��B
�	B
w�B
h>B
W�B
CaB
8B
4B
$�B
FB
B
�B
�B	��B	� B	ΥB	�UB	�B	��B	�&B	��B	h>B	V�B	IlB	<6B	0�B	+�B	�B	4B	�B	�B��B��B��B�>B��B��B��BѷB̘BɆB�SB�UB�*B�$B�B��B��B��B��B��B��B��B��B��B��B��B��B�~B�kB�?B�B��B|�BvzBs�Bu�Bx�Bw�Bt�BpUBlWBj0Bi*BfBd&BbB^�B\�BZ�BY�BX�BV�BU�BP�BN�BN�BM�BK�BH�BFYBCGB@4B?HB<6B<B9	B8B5�B5B1�B0�B/�B/�B+�B*�B)�B(�B&�B$�B!|B �B!�B�BjBpBjB�B�B�BjB�B~B�B�BjB]B!�B%�B%�B&�B%�B(�B)�B)�B*�B+�B+�B*�B+�B+�B/�B/�B0�B1�B3�B3�B4B6B7B7B7B8B8B9$B9	B9$B:B<B=<B="B?HB?.B@4B?.BEmBGzBI�BJrBL~BO�BP�BQ�BR�BS�BS�BT�BT�BU�BY�B[�B\�B]�B]�B_�BaBc Bc Bd&Bd&BiDBj0Bk6Bl=BlWBr|Bw�By�B{�B|�B~�B��B��B��B��B�	B�	B��B�B�B�B�2B�_B�]B�|B��B��B��B��B��B�B�GB�tB�~BѝBյB��B��B�B��B�B�B�=B�vB��B��B��B��B��B	�B	�B	 B	,B	MB	QB	jB	!|B	"�B	$�B	%�B	%�B	&�B	'�B	*�B	/�B	0�B	2�B	7B	9	B	>BB	>(B	?.B	@4B	BAB	FtB	KxB	O�B	P�B	Q�B	R�B	R�B	S�B	U�B	U�B	U�B	Y�B	Z�B	_�B	cB	e,B	i*B	lWB	s�B	u�B	utB	utB	w�B	z�B	��B	��B	��B	�	B	��B	��B	�B	�:B	�,B	�,B	�,B	�MB	�9B	�?B	�eB	�KB	�eB	�KB	�KB	�eB	�WB	�]B	�dB	��B	�jB	��B	�pB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�0B	�B	�B	�BB	�[B	�gB	�MB	�MB	�mB	�YB	�zB	�fB	ȀB	�lB	ʌB	�xB	�xB	�xB	�~B	ΊB	ΊB	ΥB	ϑB	ϑB	ЗB	бB	��B	ԯB	ԯB	��B	յB	��B	ּB	ּB	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�,B	�2B	�2B	�8B	�$B	�DB	�6B	�6B	�QB	�=B	�=B	�=B	�=B	�]B	�OB	�OB	�OB	�[B	�[B	�|B	�|B	�hB	�tB	�tB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
	�B
	�B

	B

�B

�B
�B
�B
B
B
B
B
B
B
(B
B
(B
(B
B
B
B
B
B
(B
B
.B
B
B
B
:B
 B
@B
&B
,B
@B
FB
&B
,B
,B
MB
MB
9B
YB
?B
YB
EB
KB
KB
eB
KB
eB
QB
qB
WB
WB
qB
WB
WB
qB
qB
xB
xB
~B
~B
dB
�B
jB
�B
�B
jB
�B
�B
�B
jB
pB
�B
�B
pB
pB
 vB
 �B
 vB
 vB
!|B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
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
)�B
)�B
)�B
*�B
+�B
+�B
+�B
,�B
-�B
-�B
-�B
.�B
/�B
/�B
/�B
/�B
/�B
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
4B
3�B
3�B
4B
4�B
6B
6B
7B
6�B
6�B
6�B
7B
8B
8B
8B
8B
8B
8B
8B
9$B
9	B
9	B
9$B
:B
:B
:*B
:B
;0B
;B
;B
<B
<6B
<B
<B
<B
="B
="B
="B
=<B
>(B
>BB
?.B
?.B
@4B
@4B
@4B
@4B
AUB
AUB
A;B
A;B
AUB
AUB
BAB
BAB
CaB
CaB
CGB
CGB
CGB
CGB
CGB
CGB
CaB
DgB
DgB
DMB
DgB
DMB
DgB
ESB
EmB
EmB
FYB
FYB
FYB
FYB
FtB
FYB
FYB
FYB
G_B
G_B
G_B
HfB
H�B
H�B
H�B
HfB
HfB
IlB
IlB
I�B
IlB
JrB
J�B
J�B
JrB
KxB
KxB
K�B
K�B
K�B
KxB
K�B
KxB
L�B
L~B
L�B
L~B
L�B
L~B
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
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
S�B
R�B
R�B
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
T�B
T�B
T�B
T�B
T�B
U�B
U�B
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
W�B
W�B
X�B
X�B
X�B
X�B
X�B
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
\�B
\�B
\�B
\�B
]�B
\�B
^B
^�B
_B
^�B
^�B
^�B
_B
_B
^�B
_�B
_�B
`B
`B
`�B
aB
`�B
a�B
a�B
cB
c B
cB
c B
c B
c B
dB
dB
d&B
d&B
d&B
dB
e,B
e,B
e,B
e,B
eB
e,B
eB
e,B
f2B
fB
f2B
gB
gB
gB
gB
gB
g8B
gB
gB
gB
h>B
h$B
h$B
h$B
h>B
h$B
iDB
i*B
iDB
i*B
i*B
iDB
iDB
iDB
j0B
j0B
j0B
j0B
j0B
jKB
jKB
kQB
k6B
k6B
kQB
k6B
l=B
mCB
mCB
mCB
m]B
m]B
mCB
m]B
mCB
m]B
m]B
mCB
mCB
m]B
mCB
nIB
oO1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.53(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201812020042042018120200420420181202004204201812030036472018120300364720181203003647JA  ARFMdecpA19c                                                                20181126213624  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181126123707  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181126123708  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181126123708  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181126123709  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181126123709  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181126123709  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181126123709  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181126123710  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181126123710                      G�O�G�O�G�O�                JA  ARUP                                                                        20181126125515                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181126153902  CV  JULD            G�O�G�O�FĜ�                JM  ARCAJMQC2.0                                                                 20181201154204  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181201154204  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181202153647  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                