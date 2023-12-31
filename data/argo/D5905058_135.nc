CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-04-02T03:43:52Z creation;2019-04-02T03:43:56Z conversion to V3.1;2019-12-23T06:04:37Z update;     
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190402034352  20200120021524  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0675_135                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @ز�9D��1   @ز���� @8�5?|��c6�zxl"1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @9��@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�FfD�c311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @dz�@�p�@�p�A
�RA*�RALQ�Aj�RA�\)A�\)A�\)A�\)A�\)A�\)A�\)A�\B�B
�B�B�B"�B*�B2�B:�BB�BJ�BR�BZ�Bb�Bj�Br�Bz�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�#�B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
B�W
C ��C��C��C��C��C
��C��C��C��C��C��C��C��C��C��C��C ��C"��C$��C&��C(��C*��C,��C.��C0��C2��C4��C6��C8��C:��C<��C>��C@��CB��CD��CF��CH��CJ��CL��CN��CP��CR��CT��CV��CX��CZ��C\��C^��C`��Cb��Cd��Cf��Ch��Cj��Cl��Cn��Cp��Cr��Ct��Cv��Cx��Cz��C|��C~��C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�b�C�b�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�C�U�D *�D ��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D	*�D	��D
*�D
��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D��D*�D�HD*�D��D*�D��D *�D ��D!*�D!��D"*�D"��D#*�D#��D$*�D$��D%*�D%��D&*�D&��D'*�D'��D(*�D(��D)*�D)��D**�D*��D+*�D+��D,*�D,��D-*�D-��D.*�D.��D/*�D/��D0*�D0��D1*�D1��D2*�D2��D3*�D3��D4*�D4��D5*�D5��D6*�D6��D7*�D7��D8*�D8��D9*�D9��D:*�D:��D;*�D;��D<*�D<��D=*�D=��D>*�D>��D?*�D?��D@*�D@��DA*�DA��DB*�DB��DC*�DC��DD*�DD��DE*�DE��DF*�DF��DG*�DG��DH*�DH��DI*�DI��DJ*�DJ��DK*�DK��DL*�DL��DM*�DM��DN*�DN��DO*�DO��DP*�DP��DQ*�DQ��DR*�DR��DS*�DS��DT*�DT��DU*�DU��DV*�DV��DW*�DW��DX*�DX��DY*�DY��DZ*�DZ��D[*�D[��D\*�D\��D]*�D]��D^*�D^��D_*�D_��D`*�D`��Da*�Da��Db*�Db��Dc*�Dc��Dd*�Dd��De*�De��Df*�Df��Dg*�Dg��Dh*�Dh��Di*�Di��Dj*�Dj��Dk*�Dk��Dl*�Dl��Dm*�Dm��Dn*�Dn��Do*�Do��Dp*�Dp��Dq*�Dq��Dr*�Dr��Ds*�Ds��Dt*�Dt��Du*�Du��Dv*�Dv��Dw*�Dw��Dx*�Dx��Dy*�Dy��Dz*�Dz��D{*�D{��D|*�D|��D}*�D}��D~*�D~��D*�D��D�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�R=D��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD�ؤD��D�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqDqD��qD�qD�UqDÕqD��qD�qD�UqDĕqD��qD�qD�UqDŕqD��qD�qD�UqDƕqD��qD�qD�UqDǕqD��qD�qD�UqDȕqD��qD�qD�UqDɕqD��qD�qD�UqDʕqD��qD�qD�UqD˕qD��qD�qD�UqD̕qD��qD�qD�UqD͕qD��qD�qD�UqDΕqD��qD�qD�UqDϕqD��qD�qD�UqDЕqD��qD�qD�UqDѕqD��qD�qD�UqDҕqD��qD�qD�UqDӕqD��qD�qD�UqDԕqD��qD�qD�UqDՕqD��qD�qD�UqD֕qD��qD�qD�UqDוqD��qD�qD�UqDؕqD��qD�qD�UqDٕqD��qD�qD�UqDڕqD��qD�qD�UqDەqD��qD�qD�UqDܕqD��qD�qD�UqDݕqD��qD�qD�UqDޕqD��qD�qD�UqDߕqD��qD�qD�UqD��qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD�qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD�qD�UqD��qD��qD��D�[�D�x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�JA�VA�{A��A��\A��+A�z�A�hsA�ffA�ffA�\)A�I�A�/A�JA��
A��!A��A�33A�JA��mA��
A���A�ȴA�ĜA�A���A���A���A��wA��wA��wA��RA��9A���A�hsA���A�(�A�v�A��^A��^A�9XA�dZA��-A���A��/A��wA�p�A�C�A��A�~�A��;A��A��9A�=qA��A�A�A�ZA���A�A���A�"�A�C�A��!A��A��HA� �A���A��A���A�~�A��HA�hsA�ĜA�A�oA�~�A��jA���A�x�A��A�l�A�M�A��A�C�A�1'A��#A�FA{?}Ay��Axn�Av5?As��Ao�mAj�Ah�9Ae�TAd1'Ab-A`1A_VA]ƨA\{AZ9XAW\)AT�AR�AP^5AO33AN1'AL�AI�AHr�AGdZAG&�AGXAGC�AFbAC�
AB��ABbAA+A@��A@ZA@=qA>��A<�+A;"�A9oA7�FA6^5A5dZA4�\A3A1A/ƨA.��A.E�A-�
A-C�A+O�A*��A*-A)�^A)��A)\)A(�RA(�uA(Q�A'VA$ffA#�A#�-A#33A"��A"A!�
A ��A�#A��A+A��An�AI�Ar�A�yA��A5?AhsA��A��A{A^5A�;A�A�wA��A �A�A7LA��A-A��A�+AffA=qAA�^A
z�AZA�
A��A��AE�A��A�A��AbNA�/A�A�A �9A ��A ��A �DA 1'@�S�@�J@���@�v�@��-@�\)@�hs@��@��
@���@�A�@�t�@@��-@�@���@�t�@�+@�\@���@�j@���@�@��
@���@��@��@�%@��@�hs@�dZ@���@�{@��`@��;@�t�@ҏ\@�@�G�@Гu@��@���@�z�@�1'@���@�n�@��@ɩ�@��`@Ȭ@�r�@ǥ�@ƸR@ź^@���@ģ�@�j@�dZ@+@��@��`@��@���@�t�@�{@�V@��D@�  @�\)@��y@�J@��h@��w@�x�@�I�@�
=@�p�@��/@�|�@���@��y@��@���@�M�@�@��@�7L@���@��j@��9@��9@��@��@���@�9X@��@�S�@���@��7@�j@�\)@�@��H@�E�@�`B@�&�@��u@�dZ@���@�v�@�@��@��#@��#@��#@��^@�x�@�X@�7L@�V@���@�(�@��m@���@�dZ@�33@�o@��@�=q@��T@��h@�hs@�X@�7L@�&�@��@��@��j@��u@�9X@��m@�|�@�K�@�o@���@�ff@�V@��^@�&�@���@��@��/@���@�Z@��;@���@�\)@�"�@�
=@��y@���@��+@�ff@�{@�7L@���@��D@�r�@�Q�@�I�@�A�@�1@���@�|�@���@�$�@��@���@���@��7@�`B@���@��@��u@�z�@�I�@�(�@�b@���@��w@��P@�\)@���@���@�n�@�M�@�J@�@��7@�`B@�?}@���@��j@���@���@���@���@���@�r�@�Z@�  @��
@��F@��@���@�l�@�S�@�;d@�33@��@���@��@��y@��y@��y@��H@��@��!@��+@�v�@��\@�~�@�^5@�^5@�M�@�M�@�E�@�=q@�5?@�@��^@���@��@�x�@�hs@�G�@��@���@��@��/@���@���@�bN@�A�@�9X@�9X@�9X@�9X@��@�  @��m@�ƨ@���@��P@�dZ@�S�@�K�@�ȴ@���@���@���@��+@�~�@�^5@�5?@�-@�$�@��@�@���@��7@�x�@�`B@�?}@�&�@���@��/@�Ĝ@��u@�Q�@�b@���@���@�ƨ@���@��P@�|�@�|�@�dZ@�C�@�C�@�"�@���@��!@�^5@�-@��^@��h@�?}@��@��@��@��@��@�V@��@��@��`@���@�j@�w@~�+@}�T@}��@}p�@|�/@{��@{C�@z�H@z��@z^5@z=q@zJ@y��@xĜ@wK�@v�@v�R@vff@vE�@u�@u@up�@t�@t��@s�
@s@r��@r=q@q�^@q�7@qhs@p�9@pQ�@o�@o�P@o;d@n�@n�+@n5?@mp�@l�/@l�@lj@l(�@k��@kC�@k@j��@i�#@ix�@iX@i%@h��@h �@g��@g��@gl�@gl�@g;d@f�@f��@f@e�-@ep�@eO�@d�@d��@dj@dI�@dI�@d1@c��@cS�@co@b�\@b^5@bM�@a�@a��@a��@a��@ax�@a7L@a�@`��@`�@`r�@`r�@`A�@_�;@_�@_�P@_K�@^�y@^ȴ@^��@^��@^ff@^@]�-@]�@]?}@\��@\�@\j@\I�@\(�@\�@\1@[��@[t�@Z�@Z-@Y��@Y�@X1'@W�w@W;d@V�R@V��@V�+@VV@V5?@U��@U�@U/@UV@T��@T�D@TZ@T(�@S��@SC�@R�@R��@R�\@Rn�@R=q@RJ@Q�#@Q7L@PĜ@P��@P�u@PQ�@O��@O�@OK�@N�R@Nv�@M�-@M�@L��@L�@L��@L��@LI�@Kƨ@K�F@K��@K@J�\@JM�@J�@I�@I�^@IX@H��@H��@H1'@H �@H  @G�w@Gl�@G;d@G�@F�+@F$�@E�T@E`B@E�@D��@Dj@D9X@C��@C��@B�@B��@BM�@A��@A��@Ax�@AX@AG�@AG�@A&�@@�9@@A�@@  @?�w@?��@?�P@?|�@?K�@?
=@>��@>E�@>$�@>{@>@=@=p�@=O�@=/@=V@<�/@<�j@<z�@<I�@<I�@;��@;dZ@;33@:�@:��@:n�@9�@9�#@9��@9hs@9&�@8��@8Ĝ@8��@8r�@8A�@7�@7�@7|�@7+@6�@6��@6E�@6$�@6{@5��@5��@5�@5O�@5�@4��@4��@4�j@4��@4Z@4(�@4(�@3ƨ@3t�@3o@2��@2^5@2=q@2J@1�^@1��@1��@1��@1�7@17L@1�@1%@0Ĝ@0�u@0A�@0b@/�@/�@/;d@.�y@.�R@.��@.v�@.5?@-�T@-��@-p�@-?}@,��@,�j@,��@,I�@,1@+�m@+��@+t�@+dZ@+C�@*��@*��@*�\@*n�@*^5@*M�@*J@)��@)x�@)&�@(��@(�u@(bN@(A�@(  @'�@'��@'�@'l�@'
=@&ȴ@&��@&��@&�+@&v�@&V@&E�@%�-@%�@%�@%?}@$��@$��@$�D@$(�@#�m@#�F@#��@#dZ@#"�@"�@"��@"�\@"^5@"=q@"-@!��@!��@ �u@  �@�w@��@|�@+@��@��@v�@ff@E�@��@�h@/@�D@Z@(�@��@�
@�F@�@S�@33@o@o@o@@�@��@~�@M�@J@�#@x�@X@G�@&�@�`@�u@�@Q�@ �@�@�@�P@\)@;d@�@
=@�y@��@v�@E�@{@�T@@�@p�@p�@`B@?}@/@��@��@��@�D@z�@j@(�@1@�m@�F@S�@o@�H@�\@n�@^5@=q@�@�@�^@��@x�@hs@%@Ĝ@Ĝ@��@�@Q�@1'@b@��@�P@K�@+@
=@�y@v�@E�@$�@�@�-@�-@��@��@p�@�@�@z�@Z@1@�
@�F@��@t�@33@@
�H@
�!@
~�@
^5@
=q@
-@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�JA�VA�{A��A��\A��+A�z�A�hsA�ffA�ffA�\)A�I�A�/A�JA��
A��!A��A�33A�JA��mA��
A���A�ȴA�ĜA�A���A���A���A��wA��wA��wA��RA��9A���A�hsA���A�(�A�v�A��^A��^A�9XA�dZA��-A���A��/A��wA�p�A�C�A��A�~�A��;A��A��9A�=qA��A�A�A�ZA���A�A���A�"�A�C�A��!A��A��HA� �A���A��A���A�~�A��HA�hsA�ĜA�A�oA�~�A��jA���A�x�A��A�l�A�M�A��A�C�A�1'A��#A�FA{?}Ay��Axn�Av5?As��Ao�mAj�Ah�9Ae�TAd1'Ab-A`1A_VA]ƨA\{AZ9XAW\)AT�AR�AP^5AO33AN1'AL�AI�AHr�AGdZAG&�AGXAGC�AFbAC�
AB��ABbAA+A@��A@ZA@=qA>��A<�+A;"�A9oA7�FA6^5A5dZA4�\A3A1A/ƨA.��A.E�A-�
A-C�A+O�A*��A*-A)�^A)��A)\)A(�RA(�uA(Q�A'VA$ffA#�A#�-A#33A"��A"A!�
A ��A�#A��A+A��An�AI�Ar�A�yA��A5?AhsA��A��A{A^5A�;A�A�wA��A �A�A7LA��A-A��A�+AffA=qAA�^A
z�AZA�
A��A��AE�A��A�A��AbNA�/A�A�A �9A ��A ��A �DA 1'@�S�@�J@���@�v�@��-@�\)@�hs@��@��
@���@�A�@�t�@@��-@�@���@�t�@�+@�\@���@�j@���@�@��
@���@��@��@�%@��@�hs@�dZ@���@�{@��`@��;@�t�@ҏ\@�@�G�@Гu@��@���@�z�@�1'@���@�n�@��@ɩ�@��`@Ȭ@�r�@ǥ�@ƸR@ź^@���@ģ�@�j@�dZ@+@��@��`@��@���@�t�@�{@�V@��D@�  @�\)@��y@�J@��h@��w@�x�@�I�@�
=@�p�@��/@�|�@���@��y@��@���@�M�@�@��@�7L@���@��j@��9@��9@��@��@���@�9X@��@�S�@���@��7@�j@�\)@�@��H@�E�@�`B@�&�@��u@�dZ@���@�v�@�@��@��#@��#@��#@��^@�x�@�X@�7L@�V@���@�(�@��m@���@�dZ@�33@�o@��@�=q@��T@��h@�hs@�X@�7L@�&�@��@��@��j@��u@�9X@��m@�|�@�K�@�o@���@�ff@�V@��^@�&�@���@��@��/@���@�Z@��;@���@�\)@�"�@�
=@��y@���@��+@�ff@�{@�7L@���@��D@�r�@�Q�@�I�@�A�@�1@���@�|�@���@�$�@��@���@���@��7@�`B@���@��@��u@�z�@�I�@�(�@�b@���@��w@��P@�\)@���@���@�n�@�M�@�J@�@��7@�`B@�?}@���@��j@���@���@���@���@���@�r�@�Z@�  @��
@��F@��@���@�l�@�S�@�;d@�33@��@���@��@��y@��y@��y@��H@��@��!@��+@�v�@��\@�~�@�^5@�^5@�M�@�M�@�E�@�=q@�5?@�@��^@���@��@�x�@�hs@�G�@��@���@��@��/@���@���@�bN@�A�@�9X@�9X@�9X@�9X@��@�  @��m@�ƨ@���@��P@�dZ@�S�@�K�@�ȴ@���@���@���@��+@�~�@�^5@�5?@�-@�$�@��@�@���@��7@�x�@�`B@�?}@�&�@���@��/@�Ĝ@��u@�Q�@�b@���@���@�ƨ@���@��P@�|�@�|�@�dZ@�C�@�C�@�"�@���@��!@�^5@�-@��^@��h@�?}@��@��@��@��@��@�V@��@��@��`@���@�j@�w@~�+@}�T@}��@}p�@|�/@{��@{C�@z�H@z��@z^5@z=q@zJ@y��@xĜ@wK�@v�@v�R@vff@vE�@u�@u@up�@t�@t��@s�
@s@r��@r=q@q�^@q�7@qhs@p�9@pQ�@o�@o�P@o;d@n�@n�+@n5?@mp�@l�/@l�@lj@l(�@k��@kC�@k@j��@i�#@ix�@iX@i%@h��@h �@g��@g��@gl�@gl�@g;d@f�@f��@f@e�-@ep�@eO�@d�@d��@dj@dI�@dI�@d1@c��@cS�@co@b�\@b^5@bM�@a�@a��@a��@a��@ax�@a7L@a�@`��@`�@`r�@`r�@`A�@_�;@_�@_�P@_K�@^�y@^ȴ@^��@^��@^ff@^@]�-@]�@]?}@\��@\�@\j@\I�@\(�@\�@\1@[��@[t�@Z�@Z-@Y��@Y�@X1'@W�w@W;d@V�R@V��@V�+@VV@V5?@U��@U�@U/@UV@T��@T�D@TZ@T(�@S��@SC�@R�@R��@R�\@Rn�@R=q@RJ@Q�#@Q7L@PĜ@P��@P�u@PQ�@O��@O�@OK�@N�R@Nv�@M�-@M�@L��@L�@L��@L��@LI�@Kƨ@K�F@K��@K@J�\@JM�@J�@I�@I�^@IX@H��@H��@H1'@H �@H  @G�w@Gl�@G;d@G�@F�+@F$�@E�T@E`B@E�@D��@Dj@D9X@C��@C��@B�@B��@BM�@A��@A��@Ax�@AX@AG�@AG�@A&�@@�9@@A�@@  @?�w@?��@?�P@?|�@?K�@?
=@>��@>E�@>$�@>{@>@=@=p�@=O�@=/@=V@<�/@<�j@<z�@<I�@<I�@;��@;dZ@;33@:�@:��@:n�@9�@9�#@9��@9hs@9&�@8��@8Ĝ@8��@8r�@8A�@7�@7�@7|�@7+@6�@6��@6E�@6$�@6{@5��@5��@5�@5O�@5�@4��@4��@4�j@4��@4Z@4(�@4(�@3ƨ@3t�@3o@2��@2^5@2=q@2J@1�^@1��@1��@1��@1�7@17L@1�@1%@0Ĝ@0�u@0A�@0b@/�@/�@/;d@.�y@.�R@.��@.v�@.5?@-�T@-��@-p�@-?}@,��@,�j@,��@,I�@,1@+�m@+��@+t�@+dZ@+C�@*��@*��@*�\@*n�@*^5@*M�@*J@)��@)x�@)&�@(��@(�u@(bN@(A�@(  @'�@'��@'�@'l�@'
=@&ȴ@&��@&��@&�+@&v�@&V@&E�@%�-@%�@%�@%?}@$��@$��@$�D@$(�@#�m@#�F@#��@#dZ@#"�@"�@"��@"�\@"^5@"=q@"-@!��@!��@ �u@  �@�w@��@|�@+@��@��@v�@ff@E�@��@�h@/@�D@Z@(�@��@�
@�F@�@S�@33@o@o@o@@�@��@~�@M�@J@�#@x�@X@G�@&�@�`@�u@�@Q�@ �@�@�@�P@\)@;d@�@
=@�y@��@v�@E�@{@�T@@�@p�@p�@`B@?}@/@��@��@��@�D@z�@j@(�@1@�m@�F@S�@o@�H@�\@n�@^5@=q@�@�@�^@��@x�@hs@%@Ĝ@Ĝ@��@�@Q�@1'@b@��@�P@K�@+@
=@�y@v�@E�@$�@�@�-@�-@��@��@p�@�@�@z�@Z@1@�
@�F@��@t�@33@@
�H@
�!@
~�@
^5@
=q@
-@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
Q�B
P�B
P�B
O�B
L�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
J�B
I�B
I�B
L�B
O�B
S�B
YB
]/B
`BB
cTB
e`B
gmB
gmB
jB
l�B
n�B
n�B
t�B
v�B
}�B
��B
�BBJB(�BC�BffB��B��B�HB�B��B��B%B1B
=BJBPB�B�B�B"�B �B{B	7B�B��B��B��B�B�BiyBM�B%�B&�BL�B=qB6FB+B#�B�BDB
��B
ƨB
��B
�B
e`B
PB
!�B
!�B
VB
B	��B	�BB	�jB	�B	��B	�uB	�B	iyB	D�B	-B	�B	B��B�B�ZB�)B��BɺB�}B�9B�B��B�{B�\B�7B�B~�B�B�=B��B�B��B��B�oB�\B�VB�PB�JB�=B�JB�JB�\B�hB�\B�=B�%B�B}�Bu�Bt�Br�Bp�Bm�Bl�Bk�BhsBjBjBiyBjBk�Bk�BjBm�BffBe`BdZBcTBaHBcTBiyBl�Bm�Bl�Bn�Bm�Bm�Bo�Bt�B}�B�=B�hB�oB�hB�VB�+Bm�BVBP�BN�B?}B;dB8RB7LB6FB5?B49B33B2-B2-B1'B/B0!B-B,B+B,B+B)�B)�B(�B(�B)�B'�B(�B(�B(�B(�B(�B(�B(�B(�B)�B)�B(�B+B+B+B-B/B.B.B.B.B.B-B-B-B,B+B,B-B,B-B,B+B-B,B,B/B0!B0!B1'B33B5?B6FB8RB9XB9XB:^B?}B@�BA�BA�BD�BD�BE�BF�BG�BH�BH�BI�BK�BN�BO�BP�BP�BVBVB^5B`BB_;B_;B^5BdZBdZBdZBdZBffBl�Bt�B|�B{�Bv�By�B~�B�B�%B�DB�PB�PB�PB�PB�bB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�jB�}B��BǮB��B��B��B�B�#B�)B�;B�HB�TB�ZB�`B�mB�yB�B�B�B�B��B��B��B	B	B	B	+B	VB	hB	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	(�B	+B	-B	0!B	49B	49B	9XB	=qB	>wB	>wB	?}B	@�B	B�B	E�B	G�B	H�B	J�B	K�B	L�B	M�B	O�B	O�B	Q�B	YB	]/B	^5B	_;B	`BB	`BB	`BB	bNB	dZB	gmB	m�B	r�B	s�B	t�B	v�B	w�B	y�B	z�B	{�B	{�B	|�B	}�B	~�B	�B	�B	�%B	�1B	�DB	�JB	�VB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�3B	�9B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�?B	�FB	�LB	�RB	�XB	�^B	�jB	�qB	�wB	�}B	�}B	�}B	�}B	��B	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�fB	�sB	�sB	�yB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
	7B
	7B

=B

=B
DB
DB
DB
JB
PB
PB
VB
VB
VB
\B
\B
bB
bB
hB
hB
hB
oB
oB
uB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
!�B
!�B
!�B
"�B
#�B
#�B
$�B
$�B
%�B
%�B
%�B
%�B
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
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
,B
+B
,B
,B
-B
-B
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
1'B
1'B
1'B
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
5?B
6FB
6FB
6FB
6FB
7LB
7LB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
9XB
9XB
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
=qB
=qB
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
@�B
A�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
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
C�B
C�B
C�B
C�B
C�B
C�B
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
F�B
F�B
F�B
F�B
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
O�B
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
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
T�B
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
XB
XB
XB
XB
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
aHB
aHB
bNB
bNB
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
l�B
l�B
l�B
l�B
l�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
Q�B
P�B
P�B
O�B
L�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
L�B
J�B
I�B
I�B
L�B
O�B
S�B
X�B
\�B
`B
c B
e,B
g8B
g8B
jKB
lWB
ncB
ncB
t�B
vzB
}�B
��B
�BB(�BCaBf2B��BʌB�B�|B��B��B�B�B
	B�BB_BeBYB"�B �BFB	B�B�OB��B�xB��B��BiDBM�B%�B&�BL�B="B6B*�B#�BQBB
�B
�tB
��B
��B
e,B
B
!�B
!�B
B
�B	�tB	�B	�B	��B	��B	�&B	��B	i*B	DgB	,�B	KB	�B�zB�KB�&B��BϑB�lB�.B��B��B�vB�,B�B�B��B~�B��B��B�]B��B��B�KB� B�B�B�B�B��B��B��B�B�B�B��B��B��B}�ButBtnBraBpoBm]Bl=BkQBh$Bj0Bj0Bi*Bj0Bk6Bk6Bj0BmCBfBeBdBcB`�BcBi*Bl=BmCBl=BncBmCBmCBoOBtnB}�B��B�B� B�B�B��BmCBU�BP�BN�B?.B;B8B6�B5�B4�B3�B2�B1�B1�B0�B.�B/�B,�B+�B*�B+�B*�B)�B)�B(�B(�B)�B'�B(�B(�B(�B(�B(�B(�B(�B(�B)�B)�B(�B*�B*�B*�B,�B.�B-�B-�B-�B-�B-�B,�B,�B,�B+�B*�B+�B,�B+�B,�B+�B*�B,�B+�B+�B.�B/�B/�B0�B2�B4�B5�B8B8�B9	B:B?B@BA;BA;BD3BDMBESBF?BGEBHKBHKBIRBKxBN�BO�BP}BP}BU�BU�B]�B_�B^�B^�B]�BdBdBdBdBfBl=BtnB|�B{�BvzByrB~�B��B��B��B�B��B��B��B�B� B�B�2B�?B�EB�KB�1B�KB�KB�KB�]B�\B�vB��B��B��B�B�.B�4B�_B̈́BЗBөBרB��B��B��B��B��B�B��B�B�*B�0B�"B�IB�[B��B��B��B	 �B	�B	�B	�B	�B	 B	B	9B	9B	?B	+B	+B	1B	WB	dB	!bB	$tB	(�B	*�B	,�B	/�B	3�B	3�B	8�B	="B	>(B	>(B	?B	@4B	BAB	E9B	GEB	HKB	JXB	KxB	L~B	M�B	OvB	O�B	Q�B	X�B	\�B	]�B	^�B	_�B	_�B	_�B	a�B	c�B	gB	m)B	rGB	shB	tTB	vzB	wfB	y�B	z�B	{B	{B	|�B	}�B	~�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�,B	�9B	�+B	�QB	�=B	�jB	�vB	�bB	�|B	�|B	�bB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�(B	�.B	�B	�B	�.B	�4B	�-B	�MB	�SB	�9B	�9B	�YB	�_B	�_B	�KB	�XB	�xB	�~B	̈́B	ΊB	�pB	ΊB	ΊB	�pB	�pB	ϑB	�}B	ЗB	уB	ңB	өB	ԯB	ԯB	רB	��B	��B	خB	��B	ٴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�$B	�$B	�*B	�$B	�B	�0B	�0B	�B	�6B	�6B	�B	�"B	�"B	�CB	�/B	�IB	�OB	�5B	�AB	�AB	�[B	�[B	�[B	�GB	�aB	�nB	�nB	�TB	�TB	�nB	�zB	��B	��B	�xB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	�B
	�B

�B

�B

�B
�B
B
B
�B
�B
B
B
B
�B
B
B
 B
B
 B
 B
&B
&B
,B
2B
B
B
2B
2B
B
2B
B
B
9B
9B
?B
$B
?B
?B
EB
+B
EB
KB
KB
KB
QB
7B
QB
7B
WB
=B
WB
=B
]B
]B
]B
]B
]B
]B
dB
dB
dB
IB
IB
OB
OB
jB
OB
pB
pB
pB
VB
pB
VB
VB
pB
VB
 \B
 \B
!|B
!|B
!bB
"�B
#�B
#nB
$�B
$�B
%�B
%�B
%�B
%zB
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
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
*�B
*�B
*�B
*�B
+�B
*�B
+�B
+�B
,�B
,�B
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
0�B
0�B
0�B
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
4�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
7�B
8B
7�B
9	B
9	B
8�B
8�B
9	B
9	B
9	B
:B
:B
;B
;B
;B
;B
;B
;B
;B
;B
<B
<B
<B
<B
<B
<B
<B
<B
<B
="B
=B
="B
="B
="B
>(B
>B
>B
?.B
?.B
?.B
?B
?.B
@B
@B
@4B
@4B
@4B
A;B
@B
@B
@4B
@4B
A B
A;B
A B
BAB
B'B
BAB
BAB
BAB
CGB
CGB
CGB
CGB
CGB
CGB
CGB
CGB
CGB
CGB
C-B
CGB
C-B
D3B
ESB
ESB
ESB
ESB
E9B
ESB
E9B
E9B
ESB
FYB
F?B
FYB
FYB
FYB
G_B
G_B
G_B
G_B
HfB
HfB
HfB
HKB
IRB
IRB
IlB
IRB
IRB
JXB
JrB
JrB
JrB
JrB
JXB
K^B
KxB
KxB
KxB
KxB
L~B
L~B
LdB
L~B
L~B
L~B
LdB
MjB
MjB
M�B
MjB
MjB
MjB
M�B
N�B
N�B
N�B
NpB
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
OvB
P�B
P�B
P�B
P}B
P}B
P}B
P�B
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
T�B
U�B
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
X�B
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
\�B
\�B
\�B
]�B
]�B
]�B
]�B
]�B
]�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
cB
cB
cB
cB
b�B
b�B
c�B
dB
dB
dB
eB
d�B
d�B
eB
eB
eB
d�B
fB
e�B
e�B
gB
gB
gB
gB
h$B
h$B
h$B
h
B
h$B
h$B
h$B
h$B
iB
i*B
i*B
j0B
j0B
j0B
j0B
j0B
kB
k6B
kB
k6B
k6B
k6B
l=B
l"B
l"B
l=B
l=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.67(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904040035472019040400354720190404003547201904050031062019040500310620190405003106JA  ARFMdecpA19c                                                                20190402123657  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190402034352  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190402034354  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190402034354  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190402034355  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190402034355  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190402034355  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190402034355  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190402034355  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190402034356                      G�O�G�O�G�O�                JA  ARUP                                                                        20190402035516                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190402154532  CV  JULD            G�O�G�O�FŔR                JM  ARCAJMQC2.0                                                                 20190403153547  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190403153547  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190404153106  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021524                      G�O�G�O�G�O�                