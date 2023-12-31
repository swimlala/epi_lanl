CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:31:58Z creation;2022-06-04T19:31:59Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604193158  20220610161506  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               vA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��	���S1   @��
9u0�@.���S��c���vȴ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�33A   AffAA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�33B���B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�fC  C  C  C  C   C"  C$  C&  C'�fC)�fC+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cu�fCx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @8Q�@~�R@\@�\)AzAAG�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B�(�B�B��]B���B�(�B�B���B���B���B�B�B���B���B���B���B���B�(�B�B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C�GC��C��C��C��C��C!��C#��C%��C'�GC)�GC+�GC-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CT{CV{CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu�GCw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AΗ$AΚAΝIAΗ�AΘ�AΛ=AΘ�AΛ=AΝ~AΟ�A΢�AΪeAάqAέ�Aή}Aί�Aί�Aα'Aβ�AδAγ�Aδ9AεAζ�AηAθAθ�Aι�Aκ^Aλ0Aλ�Aλ�Aμ6AμAνAξ�Aο�A���A��'A��'A���Aί�AΘ�Aʑ4A�!�A�[�A��HA��A��}A�֡A�چA�u�A�}�A�\�A��hA��zA���A��A�>�A�5�A��*A��A��;A��oA�R�A���A�sMA��A���A�h
A�]/A��sA�MA~Z�Az�Av_Ato ArɆAq�AoSAi�AfDgAb�4A_�9A\�AW��AUHAR�AP�AOq�AN�+AM��ALv`AJe,AH��AGiDAF*0AC?�AA#:A>�A<��A:��A9�RA9_A8��A7��A7FtA4��A4i�A2OA0�.A0IRA0L0A/��A-7�A,`BA*e�A)�zA)JA'͟A&��A$�
A$1A"5?A �A ��A Q�A��AK�A�A��AE9A$tA�MA7�A�A=A��A�Ae,A�|A�A�A\�A5�A��A��Ab�A"hA�HAD�A��A��A|�A�FA��A��AOvA�]A��AK^A�A��A�uACA
l�A
	A	��A	��A	H�A	qA˒A�MA7�A��A�}Ax�A�9A�uAl�A>BA�aA�A��A�A�AYKA��A?�A�IA ��A ��A w2A 4@��}@�u�@�%F@�o @���@�'R@�s�@�%@�L0@��m@���@�A�@�B�@�>�@��@���@���@�@O@���@��;@�7@�dZ@�ȴ@�E�@���@�O@��r@�L�@��@�s�@︻@��/@�Ft@�($@��@���@��]@�#:@��@�@��@��@���@�4@��@繌@�s�@�&@��@�J�@��@�n/@��,@�!�@�^@�F�@��P@�L@�@���@��,@��@�s�@�;@���@ݩ�@ݍP@�U�@�@@��@ڞ�@ڎ�@�]d@�'R@��@�8�@�֡@ؚ�@�@���@�x@�A @��U@ԦL@�W�@���@ғu@�o�@��	@ЯO@�Q@�@�J@��W@ͣn@�t�@�"�@�1�@�iD@ʠ�@�8�@ɔ�@�@���@�R�@��N@�`B@��@ƛ�@�q@�$�@ōP@�/@��"@���@�Ft@íC@�1�@º�@�Ft@��@�W?@��)@�H�@���@�,�@��@���@��@���@���@�U�@�:�@���@�g8@��@�4�@��]@�r�@�Z@�:*@��Z@��@���@��@��P@�\)@��P@�r�@���@�ƨ@���@�Ĝ@�c�@�/�@�	@��}@�F�@���@���@�  @���@�4�@���@��@�ѷ@���@���@�q�@�;�@���@��}@�Z@�J@��F@��@���@� �@��@�[W@�g�@�#�@��@��?@���@�q@��[@�o�@�o@���@���@�z�@�-�@��;@���@�c@��9@�	@��Z@�zx@�4@��@��1@�h�@�:�@�;�@�($@��@��@��S@�?}@��@��X@��!@��@�PH@�+k@���@�ƨ@�+�@�@��9@�Q@�@��#@��	@�c�@�dZ@�e�@�a�@�E9@�&�@��E@���@�~�@��@�J�@�!�@��@��K@��P@�J�@�(@���@�Ĝ@�>B@��@���@��"@��	@�[W@��@���@��F@���@���@�}V@�[�@��Z@���@�Q�@�!-@��U@�tT@��@���@���@�c�@�&@�ی@��x@�l�@�I�@��@��k@�Q�@��H@���@�kQ@��H@�IR@��@��@�^5@��A@��H@���@�/@�
=@��@���@��1@�p;@�A�@��@��@�_p@�2a@�(�@�ѷ@��@�Xy@���@���@�'�@���@��s@��O@���@��1@�z@�Ft@��@�˒@�v`@�T�@�J#@�C�@�?}@�.I@�Y@��s@���@�Ov@�M@��+@��Q@���@���@�+�@���@��m@�\�@��.@��K@�e,@�@@��@�YK@��@��$@�\)@�E9@�1�@�-w@��@��|@��X@�r�@�U2@�!�@�_@��@��z@��P@�B�@�%@��|@�͟@��+@�Ft@��>@���@�c@�4�@�ں@���@�H@���@�ݘ@���@�o�@��@�ߤ@���@�=q@�x@�q@W?@~�,@~��@~Q@}�@}�~@}x�@}@|bN@|b@{��@{�$@{�@z��@z@yhs@y�@x|�@x%�@wY@v�\@u��@uzx@u@@t�@tu�@s�@r�2@r�m@q�N@q*0@q�@pm�@o�@o6z@n�@nn�@n�@m�-@m[W@m�@lɆ@l��@l`�@l�@k�+@k��@k4�@ji�@i�Z@i@i|@i�@h��@h��@h>B@gn/@g�@f�"@f��@e�@ec�@eQ�@eN<@d��@dV�@d/�@d�@c�V@cn/@c4�@b��@bz@b	@ac@`��@`4n@_�w@_8@_@^ں@^5?@]c@]�@\�@\�@\_@[~�@Z͟@Z3�@Y�t@Y%F@X��@XV�@X7@W�;@W�4@WJ#@W9�@W�@V�L@V:*@U�9@U�"@U-w@T��@T  @S8@S�@S�@So@R�B@R{�@Rc @R($@Q�T@Qj@P�f@P�9@Pw�@P!@O�m@Oݘ@O�F@O
=@N�r@N	@M�=@Me,@MIR@M@@L�p@L�Y@Lm�@LPH@L~@K�@K�@@K��@KF�@J�c@J�@J��@I��@I	l@H��@H_@H,=@H�@G�g@G��@G��@G�k@G�:@GiD@GZ�@GK�@F��@F�L@FM�@F	@E�d@E��@E%F@D�O@D�4@D1'@C��@C�:@CiD@CP�@C;d@B��@B��@Bq�@B�@Aԕ@A��@Au�@AVm@A5�@A�@@�v@@�_@@c�@@M@?��@?��@?P�@>��@>?@=ϫ@=m]@=�@<Ĝ@<�@<r�@<2�@;�A@;��@;A�@:�@:E�@:4@9�@9�M@9#�@8��@8M@8x@7��@7خ@7�{@6�m@6;�@6{@5�'@4�f@4��@4D�@3خ@3�$@3�4@31�@2��@2��@2?@1��@1��@1�@0�	@0�O@0��@0e�@0�@/y�@/&@.�@.�A@.i�@.YK@.�@-�@-c�@-#�@,��@,e�@,�@+��@+˒@+��@*�@*��@*xl@*8�@)�T@)��@)q@(�?@(�_@(]d@(~@'�V@'@O@&�"@&�F@&E�@&8�@%�@%rG@%B�@%�@$��@$j@$H@$(�@$�@#�r@#�@#˒@#�f@#F�@"��@"��@"�A@"�@!��@!��@!�C@!��@!N<@!(�@!�@ �?@ ��@ 7�@�m@˒@�q@��@�@{J@b�@33@�H@��@E�@�@�@�n@��@L�@/@�@��@�@�@q@U2@��@��@��@��@_p@4�@!-@��@�@҉@��@n�@H�@.�@#:@!�@�@��@�@�@��@�M@G�@�@��@oi@7�@"h@@��@خ@��@~�@A�@ i@��@��@d�@C�@=q@)�@�@�d@?}@+@@�@ی@ѷ@��@��@�o@Xy@"h@�@�q@��@n/@RT@,�@��@��@d�@6�@$�@
�@�@@��@L�@%F@V@�@�E@�O@�@�o@>B@$@x@خ@�@�@Z�@�@��@M�@-@J@�.@�Z@�@�@�9@��@��@��@J�@�@9X@�@��@�
@�K@�@@l�@;d@)_@
��@
�6@
�A@
u%@
l�@
J�@
{@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AΗ$AΚAΝIAΗ�AΘ�AΛ=AΘ�AΛ=AΝ~AΟ�A΢�AΪeAάqAέ�Aή}Aί�Aί�Aα'Aβ�AδAγ�Aδ9AεAζ�AηAθAθ�Aι�Aκ^Aλ0Aλ�Aλ�Aμ6AμAνAξ�Aο�A���A��'A��'A���Aί�AΘ�Aʑ4A�!�A�[�A��HA��A��}A�֡A�چA�u�A�}�A�\�A��hA��zA���A��A�>�A�5�A��*A��A��;A��oA�R�A���A�sMA��A���A�h
A�]/A��sA�MA~Z�Az�Av_Ato ArɆAq�AoSAi�AfDgAb�4A_�9A\�AW��AUHAR�AP�AOq�AN�+AM��ALv`AJe,AH��AGiDAF*0AC?�AA#:A>�A<��A:��A9�RA9_A8��A7��A7FtA4��A4i�A2OA0�.A0IRA0L0A/��A-7�A,`BA*e�A)�zA)JA'͟A&��A$�
A$1A"5?A �A ��A Q�A��AK�A�A��AE9A$tA�MA7�A�A=A��A�Ae,A�|A�A�A\�A5�A��A��Ab�A"hA�HAD�A��A��A|�A�FA��A��AOvA�]A��AK^A�A��A�uACA
l�A
	A	��A	��A	H�A	qA˒A�MA7�A��A�}Ax�A�9A�uAl�A>BA�aA�A��A�A�AYKA��A?�A�IA ��A ��A w2A 4@��}@�u�@�%F@�o @���@�'R@�s�@�%@�L0@��m@���@�A�@�B�@�>�@��@���@���@�@O@���@��;@�7@�dZ@�ȴ@�E�@���@�O@��r@�L�@��@�s�@︻@��/@�Ft@�($@��@���@��]@�#:@��@�@��@��@���@�4@��@繌@�s�@�&@��@�J�@��@�n/@��,@�!�@�^@�F�@��P@�L@�@���@��,@��@�s�@�;@���@ݩ�@ݍP@�U�@�@@��@ڞ�@ڎ�@�]d@�'R@��@�8�@�֡@ؚ�@�@���@�x@�A @��U@ԦL@�W�@���@ғu@�o�@��	@ЯO@�Q@�@�J@��W@ͣn@�t�@�"�@�1�@�iD@ʠ�@�8�@ɔ�@�@���@�R�@��N@�`B@��@ƛ�@�q@�$�@ōP@�/@��"@���@�Ft@íC@�1�@º�@�Ft@��@�W?@��)@�H�@���@�,�@��@���@��@���@���@�U�@�:�@���@�g8@��@�4�@��]@�r�@�Z@�:*@��Z@��@���@��@��P@�\)@��P@�r�@���@�ƨ@���@�Ĝ@�c�@�/�@�	@��}@�F�@���@���@�  @���@�4�@���@��@�ѷ@���@���@�q�@�;�@���@��}@�Z@�J@��F@��@���@� �@��@�[W@�g�@�#�@��@��?@���@�q@��[@�o�@�o@���@���@�z�@�-�@��;@���@�c@��9@�	@��Z@�zx@�4@��@��1@�h�@�:�@�;�@�($@��@��@��S@�?}@��@��X@��!@��@�PH@�+k@���@�ƨ@�+�@�@��9@�Q@�@��#@��	@�c�@�dZ@�e�@�a�@�E9@�&�@��E@���@�~�@��@�J�@�!�@��@��K@��P@�J�@�(@���@�Ĝ@�>B@��@���@��"@��	@�[W@��@���@��F@���@���@�}V@�[�@��Z@���@�Q�@�!-@��U@�tT@��@���@���@�c�@�&@�ی@��x@�l�@�I�@��@��k@�Q�@��H@���@�kQ@��H@�IR@��@��@�^5@��A@��H@���@�/@�
=@��@���@��1@�p;@�A�@��@��@�_p@�2a@�(�@�ѷ@��@�Xy@���@���@�'�@���@��s@��O@���@��1@�z@�Ft@��@�˒@�v`@�T�@�J#@�C�@�?}@�.I@�Y@��s@���@�Ov@�M@��+@��Q@���@���@�+�@���@��m@�\�@��.@��K@�e,@�@@��@�YK@��@��$@�\)@�E9@�1�@�-w@��@��|@��X@�r�@�U2@�!�@�_@��@��z@��P@�B�@�%@��|@�͟@��+@�Ft@��>@���@�c@�4�@�ں@���@�H@���@�ݘ@���@�o�@��@�ߤ@���@�=q@�x@�q@W?@~�,@~��@~Q@}�@}�~@}x�@}@|bN@|b@{��@{�$@{�@z��@z@yhs@y�@x|�@x%�@wY@v�\@u��@uzx@u@@t�@tu�@s�@r�2@r�m@q�N@q*0@q�@pm�@o�@o6z@n�@nn�@n�@m�-@m[W@m�@lɆ@l��@l`�@l�@k�+@k��@k4�@ji�@i�Z@i@i|@i�@h��@h��@h>B@gn/@g�@f�"@f��@e�@ec�@eQ�@eN<@d��@dV�@d/�@d�@c�V@cn/@c4�@b��@bz@b	@ac@`��@`4n@_�w@_8@_@^ں@^5?@]c@]�@\�@\�@\_@[~�@Z͟@Z3�@Y�t@Y%F@X��@XV�@X7@W�;@W�4@WJ#@W9�@W�@V�L@V:*@U�9@U�"@U-w@T��@T  @S8@S�@S�@So@R�B@R{�@Rc @R($@Q�T@Qj@P�f@P�9@Pw�@P!@O�m@Oݘ@O�F@O
=@N�r@N	@M�=@Me,@MIR@M@@L�p@L�Y@Lm�@LPH@L~@K�@K�@@K��@KF�@J�c@J�@J��@I��@I	l@H��@H_@H,=@H�@G�g@G��@G��@G�k@G�:@GiD@GZ�@GK�@F��@F�L@FM�@F	@E�d@E��@E%F@D�O@D�4@D1'@C��@C�:@CiD@CP�@C;d@B��@B��@Bq�@B�@Aԕ@A��@Au�@AVm@A5�@A�@@�v@@�_@@c�@@M@?��@?��@?P�@>��@>?@=ϫ@=m]@=�@<Ĝ@<�@<r�@<2�@;�A@;��@;A�@:�@:E�@:4@9�@9�M@9#�@8��@8M@8x@7��@7خ@7�{@6�m@6;�@6{@5�'@4�f@4��@4D�@3خ@3�$@3�4@31�@2��@2��@2?@1��@1��@1�@0�	@0�O@0��@0e�@0�@/y�@/&@.�@.�A@.i�@.YK@.�@-�@-c�@-#�@,��@,e�@,�@+��@+˒@+��@*�@*��@*xl@*8�@)�T@)��@)q@(�?@(�_@(]d@(~@'�V@'@O@&�"@&�F@&E�@&8�@%�@%rG@%B�@%�@$��@$j@$H@$(�@$�@#�r@#�@#˒@#�f@#F�@"��@"��@"�A@"�@!��@!��@!�C@!��@!N<@!(�@!�@ �?@ ��@ 7�@�m@˒@�q@��@�@{J@b�@33@�H@��@E�@�@�@�n@��@L�@/@�@��@�@�@q@U2@��@��@��@��@_p@4�@!-@��@�@҉@��@n�@H�@.�@#:@!�@�@��@�@�@��@�M@G�@�@��@oi@7�@"h@@��@خ@��@~�@A�@ i@��@��@d�@C�@=q@)�@�@�d@?}@+@@�@ی@ѷ@��@��@�o@Xy@"h@�@�q@��@n/@RT@,�@��@��@d�@6�@$�@
�@�@@��@L�@%F@V@�@�E@�O@�@�o@>B@$@x@خ@�@�@Z�@�@��@M�@-@J@�.@�Z@�@�@�9@��@��@��@J�@�@9X@�@��@�
@�K@�@@l�@;d@)_@
��@
�6@
�A@
u%@
l�@
J�@
{@	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�&B	�B
�B
^�B
r�B
�(B \B2�B0�B./B>BE�BFBEBE�BM6BrB�TB��B�-Bb�Bb�B��BRB
��B
��B
��B
�zB
l=B
;dB
)�B
NB	�_B	��B	̘B	�uB	��B	�qB	�)B	�B	z�B	kQB	f�B	Y1B	C{B	9	B	0�B	)yB	%�B	#nB	�B	�B	�B	�B	^B	tB	gB�0B��B�nB�UB�qB�B��B�>B�B��B�B�B��B�TB�zB�B	B	
=B	sB	�B	�B	"B	!-B	*�B	-�B	6�B	E�B	I�B	L�B	UMB	^�B	e�B	e,B	d�B	xB	��B	�%B	�)B	��B	�-B	�'B	��B	�B	��B	�7B	�B	�"B	��B	�JB	�DB	��B	��B	��B	��B	��B	�RB	��B	�mB	�uB	�iB	~(B	�B	��B	��B	�-B	�B	�3B	��B	�[B	��B	��B	�B	��B	�+B	��B	�}B	�[B	��B	��B	��B	�,B	�,B	�B	��B	�MB	�yB	��B	�B	�jB	��B	��B	��B	��B	��B	��B	�VB	��B	��B	�B	��B	�iB	�oB	��B	�-B	��B	��B	��B	�fB	�>B	�XB	�xB	��B	�VB	�B	��B	��B	��B	��B	�iB	�4B	�B	�iB	āB	�B	�B	��B	��B	ƎB	�B	�B	��B	ƎB	ǮB	�zB	ȴB	ǮB	ƎB	ȀB	�_B	��B	�=B	��B	��B	�pB	��B	��B	ϑB	�(B	ϫB	�(B	��B	��B	��B	�B	ЗB	��B	� B	� B	�.B	�vB	�bB	ЗB	�hB	�B	бB	�:B	ѷB	�:B	�B	ҽB	�oB	� B	�aB	�?B	�B	ԯB	ՁB	��B	�CB	�B	ބB	ߤB	�B	��B	�B	�&B	��B	�FB	� B	�B	��B	�-B	�-B	�vB	�VB	�B	�ZB	�B	�2B	�$B	�$B	�B	��B	��B	�CB	�cB	��B	�B	�;B	�oB	�B	�B	�B	��B	�3B	��B	��B	��B	�B	�B	��B	�MB	�hB	��B	��B	�MB	�3B	��B	��B	��B	��B	�fB	�B	�LB	�	B	��B	��B	�B	��B	�"B	�qB	�wB	��B	�HB	��B	��B	��B	��B
 4B
 �B
 �B
 iB
 iB
 �B
;B
;B
AB
GB
B
�B
�B
�B
�B
gB
MB
gB
9B
�B
B
gB
B
�B
B
�B
?B
	RB

rB

�B

=B

�B
)B
JB
�B
dB
�B
B
B
6B
PB
�B
6B
PB
�B
�B
PB
�B
VB
B
vB
\B
\B
BB
\B
\B
�B
B
�B
 B
4B
NB
NB
�B
B
�B
�B
�B
[B
�B
uB
@B
�B
�B
aB
aB
FB
aB
�B
�B
�B
�B
�B
�B
mB
B
mB
$B
�B
+B
yB
yB
�B
�B
eB
�B
B
QB
�B
/B
~B
IB
IB
dB
~B
�B
OB
�B
�B
�B
pB
pB
pB
 B
 'B
 �B
 �B
!-B
!bB
!bB
!|B
!|B
!�B
!bB
!�B
!�B
!�B
"hB
"NB
# B
#nB
#nB
$B
#�B
#�B
$tB
$ZB
$�B
$�B
$�B
$�B
$�B
%,B
%`B
%�B
%�B
%�B
&B
&LB
&�B
&�B
'B
'�B
(sB
(�B
)*B
)DB
)*B
)_B
)�B
*KB
)�B
)�B
*0B
*�B
*�B
*�B
*�B
+B
+QB
+�B
+�B
,=B
,=B
,"B
,=B
+�B
,"B
,qB
-)B
-�B
-�B
-�B
.IB
.IB
.�B
.�B
/iB
/�B
/�B
/�B
/�B
/�B
0;B
0oB
0�B
0�B
1B
1vB
1vB
1�B
1�B
2B
2�B
2�B
2�B
2�B
33B
3hB
3�B
3�B
3�B
49B
49B
3�B
4�B
49B
4nB
4�B
4�B
4�B
4�B
5�B
5�B
6�B
6�B
6�B
72B
72B
7LB
7�B
7�B
7�B
8B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9>B
9�B
9�B
:^B
:B
:�B
:�B
;B
;0B
;B
;�B
<B
;�B
<�B
<�B
<�B
=VB
=�B
=�B
>BB
>BB
>wB
>�B
>�B
?B
?.B
?HB
?�B
?�B
?�B
?�B
@B
@�B
A B
A;B
AUB
A�B
A�B
A�B
BB
B�B
B�B
B�B
CGB
C�B
DB
C�B
C�B
DB
D�B
D�B
D�B
EB
D�B
EB
ESB
EB
EmB
E�B
E�B
F�B
F�B
G+B
G+B
GEB
G�B
H�B
H�B
H�B
HfB
H�B
I7B
I�B
I�B
J=B
J�B
KDB
KxB
K�B
K�B
LB
LJB
L0B
LJB
L0B
LdB
L�B
M�B
M�B
M�B
N�B
N�B
NpB
NVB
NVB
NpB
N�B
N�B
N�B
N�B
O\B
O�B
P.B
PHB
P}B
P�B
P�B
P�B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
RoB
RoB
RoB
R�B
R�B
SB
S&B
S[B
SuB
SuB
SuB
TFB
UB
U�B
U�B
U�B
U�B
VB
V9B
V9B
V9B
VB
VSB
VSB
V9B
V�B
V�B
W
B
WYB
WYB
WYB
W�B
XB
W�B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
]~B
]~B
]�B
^5B
^5B
^OB
^�B
^�B
_;B
_�B
_pB
_�B
_pB
_�B
`\B
`�B
`�B
aB
a�B
a�B
bB
b�B
b�B
b�B
b�B
c:B
c:B
cnB
c�B
c�B
c�B
dZB
d�B
d�B
d�B
d�B
ezB
e�B
e�B
fLB
f2B
fLB
f�B
f�B
gB
gB
gRB
g�B
h
B
h$B
h$B
hXB
h�B
iB
i_B
iyB
i�B
j0B
j�B
j�B
j�B
kB
k6B
k�B
k�B
k�B
lqB
l�B
l�B
l�B
mCB
mwB
mwB
m�B
nB
nIB
n}B
ncB
n�B
n}B
n�B
n�B
oB
oOB
oOB
o�B
p;B
poB
pUB
pUB
pUB
p�B
p�B
p�B
q[B
qvB
q�B
r-B
rGB
raB
r�B
r�B
r�B
r�B
r�B
shB
s�B
s�B
tB
tB
tnB
t�B
t�B
t�B
t�B
t�B
u%B
u?B
utB
utB
u�B
u�B
u�B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x8B
x8B
xlB
x�B
y	B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
zB
z^B
zxB
z�B
z�B
z�B
z�B
{B
z�B
{dB
|B
|B
|B
|PB
|PB
|jB
|jB
|�B
|�B
|�B
|�B
}B
}qB
}�B
}�B
}�B
}�B
}�B
~B
~]B
~]B
~wB
~�B
~�B
~�B
.B
HB
HB
cB
}B
}B
�B
�B
�B
�B
�B
�4B
�OB
��B
��B
��B
�B
�;B
��B
��B
��B
��B
�'B
��B
��B
��B
��B
��B
��B
�GB
�aB
�GB
�aB
��B
��B
��B
��B
�B
�3B
�3B
�MB
��B
��B
��B
��B
�B
�9B
�S11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�&B	�B
�B
^�B
r�B
�(B \B2�B0�B./B>BE�BFBEBE�BM6BrB�TB��B�-Bb�Bb�B��BRB
��B
��B
��B
�zB
l=B
;dB
)�B
NB	�_B	��B	̘B	�uB	��B	�qB	�)B	�B	z�B	kQB	f�B	Y1B	C{B	9	B	0�B	)yB	%�B	#nB	�B	�B	�B	�B	^B	tB	gB�0B��B�nB�UB�qB�B��B�>B�B��B�B�B��B�TB�zB�B	B	
=B	sB	�B	�B	"B	!-B	*�B	-�B	6�B	E�B	I�B	L�B	UMB	^�B	e�B	e,B	d�B	xB	��B	�%B	�)B	��B	�-B	�'B	��B	�B	��B	�7B	�B	�"B	��B	�JB	�DB	��B	��B	��B	��B	��B	�RB	��B	�mB	�uB	�iB	~(B	�B	��B	��B	�-B	�B	�3B	��B	�[B	��B	��B	�B	��B	�+B	��B	�}B	�[B	��B	��B	��B	�,B	�,B	�B	��B	�MB	�yB	��B	�B	�jB	��B	��B	��B	��B	��B	��B	�VB	��B	��B	�B	��B	�iB	�oB	��B	�-B	��B	��B	��B	�fB	�>B	�XB	�xB	��B	�VB	�B	��B	��B	��B	��B	�iB	�4B	�B	�iB	āB	�B	�B	��B	��B	ƎB	�B	�B	��B	ƎB	ǮB	�zB	ȴB	ǮB	ƎB	ȀB	�_B	��B	�=B	��B	��B	�pB	��B	��B	ϑB	�(B	ϫB	�(B	��B	��B	��B	�B	ЗB	��B	� B	� B	�.B	�vB	�bB	ЗB	�hB	�B	бB	�:B	ѷB	�:B	�B	ҽB	�oB	� B	�aB	�?B	�B	ԯB	ՁB	��B	�CB	�B	ބB	ߤB	�B	��B	�B	�&B	��B	�FB	� B	�B	��B	�-B	�-B	�vB	�VB	�B	�ZB	�B	�2B	�$B	�$B	�B	��B	��B	�CB	�cB	��B	�B	�;B	�oB	�B	�B	�B	��B	�3B	��B	��B	��B	�B	�B	��B	�MB	�hB	��B	��B	�MB	�3B	��B	��B	��B	��B	�fB	�B	�LB	�	B	��B	��B	�B	��B	�"B	�qB	�wB	��B	�HB	��B	��B	��B	��B
 4B
 �B
 �B
 iB
 iB
 �B
;B
;B
AB
GB
B
�B
�B
�B
�B
gB
MB
gB
9B
�B
B
gB
B
�B
B
�B
?B
	RB

rB

�B

=B

�B
)B
JB
�B
dB
�B
B
B
6B
PB
�B
6B
PB
�B
�B
PB
�B
VB
B
vB
\B
\B
BB
\B
\B
�B
B
�B
 B
4B
NB
NB
�B
B
�B
�B
�B
[B
�B
uB
@B
�B
�B
aB
aB
FB
aB
�B
�B
�B
�B
�B
�B
mB
B
mB
$B
�B
+B
yB
yB
�B
�B
eB
�B
B
QB
�B
/B
~B
IB
IB
dB
~B
�B
OB
�B
�B
�B
pB
pB
pB
 B
 'B
 �B
 �B
!-B
!bB
!bB
!|B
!|B
!�B
!bB
!�B
!�B
!�B
"hB
"NB
# B
#nB
#nB
$B
#�B
#�B
$tB
$ZB
$�B
$�B
$�B
$�B
$�B
%,B
%`B
%�B
%�B
%�B
&B
&LB
&�B
&�B
'B
'�B
(sB
(�B
)*B
)DB
)*B
)_B
)�B
*KB
)�B
)�B
*0B
*�B
*�B
*�B
*�B
+B
+QB
+�B
+�B
,=B
,=B
,"B
,=B
+�B
,"B
,qB
-)B
-�B
-�B
-�B
.IB
.IB
.�B
.�B
/iB
/�B
/�B
/�B
/�B
/�B
0;B
0oB
0�B
0�B
1B
1vB
1vB
1�B
1�B
2B
2�B
2�B
2�B
2�B
33B
3hB
3�B
3�B
3�B
49B
49B
3�B
4�B
49B
4nB
4�B
4�B
4�B
4�B
5�B
5�B
6�B
6�B
6�B
72B
72B
7LB
7�B
7�B
7�B
8B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9>B
9�B
9�B
:^B
:B
:�B
:�B
;B
;0B
;B
;�B
<B
;�B
<�B
<�B
<�B
=VB
=�B
=�B
>BB
>BB
>wB
>�B
>�B
?B
?.B
?HB
?�B
?�B
?�B
?�B
@B
@�B
A B
A;B
AUB
A�B
A�B
A�B
BB
B�B
B�B
B�B
CGB
C�B
DB
C�B
C�B
DB
D�B
D�B
D�B
EB
D�B
EB
ESB
EB
EmB
E�B
E�B
F�B
F�B
G+B
G+B
GEB
G�B
H�B
H�B
H�B
HfB
H�B
I7B
I�B
I�B
J=B
J�B
KDB
KxB
K�B
K�B
LB
LJB
L0B
LJB
L0B
LdB
L�B
M�B
M�B
M�B
N�B
N�B
NpB
NVB
NVB
NpB
N�B
N�B
N�B
N�B
O\B
O�B
P.B
PHB
P}B
P�B
P�B
P�B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R:B
RoB
RoB
RoB
R�B
R�B
SB
S&B
S[B
SuB
SuB
SuB
TFB
UB
U�B
U�B
U�B
U�B
VB
V9B
V9B
V9B
VB
VSB
VSB
V9B
V�B
V�B
W
B
WYB
WYB
WYB
W�B
XB
W�B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
Y1B
YB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZkB
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
]~B
]~B
]�B
^5B
^5B
^OB
^�B
^�B
_;B
_�B
_pB
_�B
_pB
_�B
`\B
`�B
`�B
aB
a�B
a�B
bB
b�B
b�B
b�B
b�B
c:B
c:B
cnB
c�B
c�B
c�B
dZB
d�B
d�B
d�B
d�B
ezB
e�B
e�B
fLB
f2B
fLB
f�B
f�B
gB
gB
gRB
g�B
h
B
h$B
h$B
hXB
h�B
iB
i_B
iyB
i�B
j0B
j�B
j�B
j�B
kB
k6B
k�B
k�B
k�B
lqB
l�B
l�B
l�B
mCB
mwB
mwB
m�B
nB
nIB
n}B
ncB
n�B
n}B
n�B
n�B
oB
oOB
oOB
o�B
p;B
poB
pUB
pUB
pUB
p�B
p�B
p�B
q[B
qvB
q�B
r-B
rGB
raB
r�B
r�B
r�B
r�B
r�B
shB
s�B
s�B
tB
tB
tnB
t�B
t�B
t�B
t�B
t�B
u%B
u?B
utB
utB
u�B
u�B
u�B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x8B
x8B
xlB
x�B
y	B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
zB
z^B
zxB
z�B
z�B
z�B
z�B
{B
z�B
{dB
|B
|B
|B
|PB
|PB
|jB
|jB
|�B
|�B
|�B
|�B
}B
}qB
}�B
}�B
}�B
}�B
}�B
~B
~]B
~]B
~wB
~�B
~�B
~�B
.B
HB
HB
cB
}B
}B
�B
�B
�B
�B
�B
�4B
�OB
��B
��B
��B
�B
�;B
��B
��B
��B
��B
�'B
��B
��B
��B
��B
��B
��B
�GB
�aB
�GB
�aB
��B
��B
��B
��B
�B
�3B
�3B
�MB
��B
��B
��B
��B
�B
�9B
�S11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105252  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604193158  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604193159  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604193159                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043206  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043206  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161506                      G�O�G�O�G�O�                