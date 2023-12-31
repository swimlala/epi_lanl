CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:45:15Z creation;2022-06-04T17:45:15Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174515  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @ر ����1   @رes��@-�I�^�dB��vȴ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�33B���B���B�  B�33B���B���B�  B�  B�  B�33B���B���B�  B�  B�33B�ffB˙�B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C��C�fC  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf�Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@~�R@�\)@�\)A�AAG�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B�(�B��]B�B���B�(�B��]B�B���B���B���B�(�B��]B�B���B���B�(�B�\)Bˏ]B���B���B���B���B���B���B���B���B���B���B���B���C {CǮC�GC��C��C	��C�GC��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C0{C2{C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Cf{Ch{Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�DvDv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�JXA�H�A�?HA�3�A�0�A�2�A�!�A���A���A��6A��9Aʲ�Aʪ�AʝIAʗ�Aʓ�Aʒ�Aʒ�Aʓ�AʔAʏ�Aʑ�Aʔ�Aʗ�Aʛ	Aʘ_AʔAʋ�Aʇ�Aʅ�Aʈ1A�tA�N�A��A�K^AʋDA�}�A�n�A�GA�D�Aɩ_A�ںA�`A�t�A�;A�S�A�L�A�{�A�g�A��pA�\A�}�A�QA��@A��A��!A�|A�уA�<A��A���A��9A�� A��A�/�A��$A�g�A�T�A���A��xA�r|A�C�A���A�hsA��SA��A���A�A�t�A�JA��}A��A|�dAys�Ax	Awg�AuYKAq�An��Ak�Af��AaC-A^�ZA]�A[��AX� AX�AW~(AU�AR�DAOSALb�AKJAJ iAHr�AE��AA�A>JA;�MA8��A6��A56A28A1�zA0�QA.خA,�AA*4A(�A'�6A'4A&\�A%�A$��A#@�A"ƨA"�qA"{�A"CA!u%A!=A ��A �An/A_pAN<A��AS&AQA�Ai�A@�A�A��A*0A �A��A�}AP�Ao�AN�AR�AݘA;A�SAc A?A�A��A��A"�A�	A��A��AJ#A��AѷA�A��A@OAU2A	AS�AϫA
�A
�aA
��A	�!A	 �A��A�A!�A�bAYKA�A��AGA]dA2aAMA��A�+AjA+A��A[WA:�A�AMAںA�gAѷA�0A\)A�A�A`�A /�@�!@��@��
@��0@���@��M@�Vm@�33@�:�@��S@�T�@���@��@�h�@�"h@��@�@�n�@���@�B�@�@��@��@���@�Ov@��@�x@���@�y>@�Vm@���@��@��@��@�RT@��@�l"@���@��@�,=@�[W@���@�w�@���@�g�@�o@��U@⠐@�+k@�@��A@�.�@��A@��}@�|@�%@ޘ_@�a|@�!@���@�@�C�@���@��z@�֡@�p�@�\�@�S�@�1@ռ�@�@O@���@ԇ�@�PH@�C�@�$@��;@�qv@��H@�Z�@�@�x�@�'�@���@ίO@�n�@�
�@�ԕ@�zx@̭�@���@ˊ	@�e�@�6z@���@�&�@���@���@ɪ�@�H�@Ȁ�@Ǚ�@��@��s@�-�@�S�@��@�kQ@�$�@��@�T�@��@��s@���@°�@�@�E�@��+@�˒@���@�w2@�!-@���@�{@��4@��@���@�^5@�3�@� �@��T@��@�G�@��@�~@��@���@��@@��B@��@�=@���@���@�g8@�=q@�ƨ@�2a@�@��@��@��@��@�[�@�7@��k@�J�@��@���@��I@�6@���@��$@�/@�d�@���@��@��O@���@�x�@�U�@��@�~�@�*�@�_@��;@�w2@�֡@��@��1@�'R@�w2@�;@���@�r�@�B[@��A@��f@��@��`@���@�d�@��@���@�f�@��@�ѷ@�s�@��)@��w@��@��{@���@��@���@���@���@�@O@�!-@��@��R@��)@�4@��O@�I�@�_@���@��0@��f@�b�@�?}@��@��@��@��]@��)@��X@��X@���@���@�y>@�Ft@�/�@��@��@�l�@�)_@�o@��@��@���@���@���@��@�X@��R@�?@���@�C�@���@��6@�q�@�!�@���@�خ@�y�@�;d@��M@���@��o@�V@��@��{@�A�@��@�W�@�1�@�$�@��o@���@�}�@�[W@�=�@��@��@���@�W�@��@��A@���@��0@���@�7L@�@@��@�^5@��@��@��@�҉@���@���@���@�`�@���@��|@�-@�_@���@��-@���@�1�@���@��Y@��@��@��@@�n/@�\)@�Vm@�:�@�"�@���@���@�`�@��@���@��@��:@�F�@�o@��f@��@��@��r@�kQ@�)�@��@�u@���@�J#@�#�@�$t@�	l@��4@�9X@�	@�1@���@��@��@��@�hs@��@��'@�w�@�)�@�u@���@���@���@�Q�@�33@��@��@���@��m@��6@��D@�K^@|�@~��@}��@}�T@}�X@|�`@|�@|�o@|2�@{�6@{=@z� @y��@y&�@x�[@xZ@w�@@w�@v��@vGE@v�@u�@u��@u:�@t�_@tD�@s�a@sP�@s@r��@r��@rO@q��@qDg@p��@p�)@p��@p4n@o�@o�F@o��@oqv@o/�@n�"@n�h@n��@m�@m`B@m/@l��@l6@k��@j�s@j�6@j�@j{@i��@i�@iL�@i�@h��@h"h@g��@g@O@g�@f�8@f��@f�@f��@fa|@f+k@f�@e��@e�@e�@d��@dr�@c��@c]�@c
=@b�@b@�@a��@a�7@a4@`�@`H@`@_�@_��@_\)@^�@^_�@^�@]��@]X@]�@]@@\�@[��@[��@Z�2@Z��@Z��@ZO@Y�N@Y�t@Yzx@Y@X�)@Xu�@W�@W|�@W�@V͟@V��@Vd�@V-@U��@U[W@UL�@U+@TĜ@Tg8@T-�@S�A@S��@S{J@Sqv@S$t@S�@R��@RW�@R	@Q@QDg@Q+@P�@P��@PPH@P@O��@O�[@N��@N�L@M�#@Mhs@MB�@M7L@M@@L��@Lc�@LC-@K�@Kl�@J�@J�!@I�@I\�@I:�@H�@H��@H~(@H�@G��@G�;@G��@Ga@GA�@G"�@F��@F��@FYK@FGE@E�@Ezx@EQ�@D�?@D�u@D�@Dh�@D�@C|�@C@B��@BM�@B	@Aϫ@A:�@A+@A�@A@@�[@?��@?��@?�@?S@>Ta@>)�@=�9@=e,@<��@<V�@<,=@;�q@;�@:��@:��@:v�@:8�@9��@9F@9�@8��@8@7=@6�@6�<@6��@6@�@5�@5j@5�@4�4@4�@4[�@47�@4G@3��@3�P@3;d@2�]@2Ov@2{@1s�@0�$@0H@0(�@01@/�K@/s@.�@.a|@.�@-�@-e,@-<6@-q@-�@-�@,�@,<�@+� @+�:@+_p@*�M@*��@*h
@)�@)*0@(�u@(l"@(M@(�@'� @'o@&�m@&��@&��@&�F@&\�@&($@&�@%��@%��@%��@%�~@%X@%F@%-w@%�@$�|@$ѷ@$��@$b@#��@#@"Q@!��@!|@ �P@ ��@ K^@�}@�@M�@�o@ԕ@k�@#�@�|@��@�I@~(@U2@ �@�+@˒@�q@b�@,�@ i@�c@��@��@�}@�1@� @��@��@1�@��@5�@��@�p@]d@$@@b@�@��@�K@�{@e�@;d@�@�@�@��@�h@�\@:*@�)@��@�@��@�=@�@��@c�@#�@�P@�/@��@`�@S�@?�@1@�}@o�@,�@S@ߤ@҉@��@Q@;�@J@ϫ@��@k�@`B@X@4@/@<6@8�@0�@�	@�I@m�@7�@2�@$@�
@�f@�4@g�@RT@H�@C�@E9@F�@C�@�@�M@�y@�s@��@_�@=q@J@�@�'@�h@��@|@j@N<@/@q@�	@�j@��@~(@6@��@l�@Z�@=@&@
�8@
�@
�@
d�@
8�@
#:@
@	�N@	�^@	�S@	a�@	O�@	0�@	&�@	�@�@�I@q@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�JXA�H�A�?HA�3�A�0�A�2�A�!�A���A���A��6A��9Aʲ�Aʪ�AʝIAʗ�Aʓ�Aʒ�Aʒ�Aʓ�AʔAʏ�Aʑ�Aʔ�Aʗ�Aʛ	Aʘ_AʔAʋ�Aʇ�Aʅ�Aʈ1A�tA�N�A��A�K^AʋDA�}�A�n�A�GA�D�Aɩ_A�ںA�`A�t�A�;A�S�A�L�A�{�A�g�A��pA�\A�}�A�QA��@A��A��!A�|A�уA�<A��A���A��9A�� A��A�/�A��$A�g�A�T�A���A��xA�r|A�C�A���A�hsA��SA��A���A�A�t�A�JA��}A��A|�dAys�Ax	Awg�AuYKAq�An��Ak�Af��AaC-A^�ZA]�A[��AX� AX�AW~(AU�AR�DAOSALb�AKJAJ iAHr�AE��AA�A>JA;�MA8��A6��A56A28A1�zA0�QA.خA,�AA*4A(�A'�6A'4A&\�A%�A$��A#@�A"ƨA"�qA"{�A"CA!u%A!=A ��A �An/A_pAN<A��AS&AQA�Ai�A@�A�A��A*0A �A��A�}AP�Ao�AN�AR�AݘA;A�SAc A?A�A��A��A"�A�	A��A��AJ#A��AѷA�A��A@OAU2A	AS�AϫA
�A
�aA
��A	�!A	 �A��A�A!�A�bAYKA�A��AGA]dA2aAMA��A�+AjA+A��A[WA:�A�AMAںA�gAѷA�0A\)A�A�A`�A /�@�!@��@��
@��0@���@��M@�Vm@�33@�:�@��S@�T�@���@��@�h�@�"h@��@�@�n�@���@�B�@�@��@��@���@�Ov@��@�x@���@�y>@�Vm@���@��@��@��@�RT@��@�l"@���@��@�,=@�[W@���@�w�@���@�g�@�o@��U@⠐@�+k@�@��A@�.�@��A@��}@�|@�%@ޘ_@�a|@�!@���@�@�C�@���@��z@�֡@�p�@�\�@�S�@�1@ռ�@�@O@���@ԇ�@�PH@�C�@�$@��;@�qv@��H@�Z�@�@�x�@�'�@���@ίO@�n�@�
�@�ԕ@�zx@̭�@���@ˊ	@�e�@�6z@���@�&�@���@���@ɪ�@�H�@Ȁ�@Ǚ�@��@��s@�-�@�S�@��@�kQ@�$�@��@�T�@��@��s@���@°�@�@�E�@��+@�˒@���@�w2@�!-@���@�{@��4@��@���@�^5@�3�@� �@��T@��@�G�@��@�~@��@���@��@@��B@��@�=@���@���@�g8@�=q@�ƨ@�2a@�@��@��@��@��@�[�@�7@��k@�J�@��@���@��I@�6@���@��$@�/@�d�@���@��@��O@���@�x�@�U�@��@�~�@�*�@�_@��;@�w2@�֡@��@��1@�'R@�w2@�;@���@�r�@�B[@��A@��f@��@��`@���@�d�@��@���@�f�@��@�ѷ@�s�@��)@��w@��@��{@���@��@���@���@���@�@O@�!-@��@��R@��)@�4@��O@�I�@�_@���@��0@��f@�b�@�?}@��@��@��@��]@��)@��X@��X@���@���@�y>@�Ft@�/�@��@��@�l�@�)_@�o@��@��@���@���@���@��@�X@��R@�?@���@�C�@���@��6@�q�@�!�@���@�خ@�y�@�;d@��M@���@��o@�V@��@��{@�A�@��@�W�@�1�@�$�@��o@���@�}�@�[W@�=�@��@��@���@�W�@��@��A@���@��0@���@�7L@�@@��@�^5@��@��@��@�҉@���@���@���@�`�@���@��|@�-@�_@���@��-@���@�1�@���@��Y@��@��@��@@�n/@�\)@�Vm@�:�@�"�@���@���@�`�@��@���@��@��:@�F�@�o@��f@��@��@��r@�kQ@�)�@��@�u@���@�J#@�#�@�$t@�	l@��4@�9X@�	@�1@���@��@��@��@�hs@��@��'@�w�@�)�@�u@���@���@���@�Q�@�33@��@��@���@��m@��6@��D@�K^@|�@~��@}��@}�T@}�X@|�`@|�@|�o@|2�@{�6@{=@z� @y��@y&�@x�[@xZ@w�@@w�@v��@vGE@v�@u�@u��@u:�@t�_@tD�@s�a@sP�@s@r��@r��@rO@q��@qDg@p��@p�)@p��@p4n@o�@o�F@o��@oqv@o/�@n�"@n�h@n��@m�@m`B@m/@l��@l6@k��@j�s@j�6@j�@j{@i��@i�@iL�@i�@h��@h"h@g��@g@O@g�@f�8@f��@f�@f��@fa|@f+k@f�@e��@e�@e�@d��@dr�@c��@c]�@c
=@b�@b@�@a��@a�7@a4@`�@`H@`@_�@_��@_\)@^�@^_�@^�@]��@]X@]�@]@@\�@[��@[��@Z�2@Z��@Z��@ZO@Y�N@Y�t@Yzx@Y@X�)@Xu�@W�@W|�@W�@V͟@V��@Vd�@V-@U��@U[W@UL�@U+@TĜ@Tg8@T-�@S�A@S��@S{J@Sqv@S$t@S�@R��@RW�@R	@Q@QDg@Q+@P�@P��@PPH@P@O��@O�[@N��@N�L@M�#@Mhs@MB�@M7L@M@@L��@Lc�@LC-@K�@Kl�@J�@J�!@I�@I\�@I:�@H�@H��@H~(@H�@G��@G�;@G��@Ga@GA�@G"�@F��@F��@FYK@FGE@E�@Ezx@EQ�@D�?@D�u@D�@Dh�@D�@C|�@C@B��@BM�@B	@Aϫ@A:�@A+@A�@A@@�[@?��@?��@?�@?S@>Ta@>)�@=�9@=e,@<��@<V�@<,=@;�q@;�@:��@:��@:v�@:8�@9��@9F@9�@8��@8@7=@6�@6�<@6��@6@�@5�@5j@5�@4�4@4�@4[�@47�@4G@3��@3�P@3;d@2�]@2Ov@2{@1s�@0�$@0H@0(�@01@/�K@/s@.�@.a|@.�@-�@-e,@-<6@-q@-�@-�@,�@,<�@+� @+�:@+_p@*�M@*��@*h
@)�@)*0@(�u@(l"@(M@(�@'� @'o@&�m@&��@&��@&�F@&\�@&($@&�@%��@%��@%��@%�~@%X@%F@%-w@%�@$�|@$ѷ@$��@$b@#��@#@"Q@!��@!|@ �P@ ��@ K^@�}@�@M�@�o@ԕ@k�@#�@�|@��@�I@~(@U2@ �@�+@˒@�q@b�@,�@ i@�c@��@��@�}@�1@� @��@��@1�@��@5�@��@�p@]d@$@@b@�@��@�K@�{@e�@;d@�@�@�@��@�h@�\@:*@�)@��@�@��@�=@�@��@c�@#�@�P@�/@��@`�@S�@?�@1@�}@o�@,�@S@ߤ@҉@��@Q@;�@J@ϫ@��@k�@`B@X@4@/@<6@8�@0�@�	@�I@m�@7�@2�@$@�
@�f@�4@g�@RT@H�@C�@E9@F�@C�@�@�M@�y@�s@��@_�@=q@J@�@�'@�h@��@|@j@N<@/@q@�	@�j@��@~(@6@��@l�@Z�@=@&@
�8@
�@
�@
d�@
8�@
#:@
@	�N@	�^@	�S@	a�@	O�@	0�@	&�@	�@�@�I@q@r�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B�gB�gB�MB�MB��B��B�-B�B��B�uB�B��B�AB�[B�[B��B��B�MB�MB��B�B�#B�<B��B�+B��B��B��B��B��B	3B	OvB	�.B	��B	�+B	��B	�B
E�B
|�B
<B
VB
�@B
��B
��B3MBg�B��B��B��B�B^B(B�BB~BB iB�B��BޞB��B�@B�!B}qB^�BM�BA�B;BB�BE9B:�B$�B
�!B
�B
��B
XEB
#�B
�B
�B	�<B	��B	�mB	��B	�RB	��B	�B	�B	x�B	aHB	J�B	D�B	>B	4�B	)�B	$tB	 �B	�B	
�B��B�tB�B�B�6B�B�B�xBƨB��B��B��B��B��B��BĶB�PB�EB�B�B�B�&B�B�|B	B	&B	FB	�B	]B	&2B	(�B	0;B	6�B	@B	?�B	=B	=�B	=qB	>�B	>B	@4B	B�B	EB	F?B	H�B	LJB	X�B	j�B	oB	B	�KB	��B	��B	��B	�B	��B	�B	�#B	��B	�vB	��B	��B	��B	�FB	��B	��B	�`B	��B	�zB	��B	�eB	��B	��B	��B	��B	�[B	��B	��B	��B	��B	��B	�cB	�uB	�4B	�gB	��B	��B	��B	��B	ևB	�vB	�B	��B	�tB	��B	�-B	�B	��B	�B	��B	�bB	�NB	��B	��B	�B	�TB	��B	�NB	��B	�)B	��B	�CB	��B	�~B	ބB	�B	ܒB	��B	�hB	�\B	��B	ݲB	�,B	��B	߾B	ܬB	�-B	��B	�B	��B	�B	�B	�8B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�:B	�4B	�-B	�bB	�-B	�4B	��B	��B	��B	�B	�B	��B	��B	�B	��B	�B	� B	�B	�hB	��B	��B	�B	�|B	�|B	�bB	��B	��B	�\B	��B	�~B	�WB	�B	��B	֡B	�2B	�2B	ԕB	�,B	��B	�[B	�[B	ӏB	�[B	�&B	�[B	�&B	��B	�B	�9B	�mB	�mB	ևB	�B	՛B	�gB	��B	�sB	�sB	�YB	�YB	�sB	��B	��B	׍B	׍B	׍B	ؓB	ٚB	��B	ٴB	�#B	��B	�)B	�]B	��B	�/B	ݘB	�B	�5B	�5B	�5B	�OB	޸B	��B	�!B	�VB	�pB	��B	�'B	��B	�HB	�B	�B	��B	�:B	�nB	�nB	�nB	��B	�B	�zB	�zB	�FB	�zB	�B	�$B	��B	��B	�_B	�yB	�B	�eB	�B	�B	�6B	�6B	�B	��B	��B	�"B	��B	�)B	��B	�B	��B	�cB	� B	��B	�OB	�;B	��B	��B	�B	�B	�|B	�aB	�B	�MB	�hB	�hB	�hB	�9B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�lB	��B	�$B	�*B	�*B	�B	��B	��B	��B	��B	�"B	�qB	�VB	�B	��B	��B	�wB	�]B	�wB	�BB	�(B	�BB	�HB
 B
 �B
UB
�B
oB
�B
[B
�B
�B
-B
{B
�B
�B
3B
3B
3B
B
�B
�B
mB
mB
�B
�B
�B
�B
	B
	�B
	�B
	7B
	RB
	�B

�B
�B
jB
6B
dB
B
�B
�B
dB
JB
0B
B
B
�B
~B
�B
~B
dB
PB
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
.B
.B
bB
�B
B
4B
hB
�B
oB
�B
�B
�B
�B
uB
,B
�B
�B
�B
�B
B
mB
�B
�B
B
�B
7B
�B
�B
#B
WB
#B
=B
�B
�B
)B
]B
�B
�B
�B
B
VB
 B
 �B
 vB
 �B
!-B
!�B
"NB
"�B
"�B
"�B
"�B
#TB
#�B
#�B
#�B
$ZB
%,B
%zB
%�B
%�B
%�B
&�B
&�B
'B
(
B
(�B
(�B
(�B
(�B
)_B
)�B
*KB
*�B
*�B
*�B
+�B
+�B
,B
,B
,B
,B
,=B
,�B
,�B
-B
-wB
.IB
/B
0B
/�B
0;B
0oB
0!B
0oB
1AB
1�B
1�B
2-B
2GB
2B
2�B
4B
4nB
4TB
49B
4�B
4�B
5ZB
5�B
6FB
6�B
6�B
7LB
7�B
7�B
7�B
7�B
8B
8B
8�B
8�B
8�B
9$B
9XB
9�B
9�B
9�B
:B
:*B
:^B
:^B
:^B
:�B
;0B
;0B
;dB
;�B
;�B
<jB
<�B
<�B
="B
=qB
=qB
=�B
=�B
=�B
>BB
>�B
>�B
>�B
?B
?cB
?�B
?�B
@ B
@B
@4B
@4B
@iB
@�B
@�B
@�B
AUB
AUB
A�B
AoB
A�B
BB
BB
BuB
B�B
B�B
B�B
B�B
B�B
CaB
C�B
C�B
DMB
D�B
D�B
D�B
D�B
EB
EmB
EmB
F%B
F%B
E�B
FtB
F�B
F�B
F�B
F�B
GB
G+B
G�B
G�B
H1B
HKB
HfB
HfB
H�B
H�B
I7B
IB
I7B
I�B
I�B
J	B
J#B
J=B
J=B
J#B
JrB
J�B
J�B
KDB
K�B
LB
LB
K�B
K�B
LJB
LdB
L�B
MPB
M�B
N�B
N�B
OvB
OvB
O\B
OBB
O�B
O�B
PB
P.B
PHB
P�B
P�B
Q B
P�B
P�B
P�B
P}B
P}B
P�B
QB
QNB
QhB
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R�B
S@B
SuB
S�B
TB
TB
TB
T,B
T{B
T{B
T�B
UMB
U�B
VmB
W
B
W$B
W
B
W
B
W
B
W�B
W�B
W�B
XEB
XyB
X�B
X�B
X�B
YeB
YeB
YeB
Y�B
Y�B
ZB
ZB
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
[qB
\B
\xB
\�B
\�B
\�B
]/B
]�B
]�B
^OB
^OB
^jB
^jB
^�B
^�B
^�B
_B
_;B
_�B
_�B
`'B
`�B
`�B
`�B
`�B
`�B
`�B
a-B
a|B
a�B
a�B
bB
bB
b4B
b4B
b4B
b4B
bhB
b�B
b�B
b�B
b�B
b�B
b�B
c B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
e,B
eFB
ezB
ezB
e�B
e�B
e�B
f2B
f2B
ffB
ffB
f�B
f�B
gB
g�B
h�B
i*B
i�B
jB
jB
j�B
j�B
j�B
k�B
l�B
mCB
m�B
m�B
nB
n/B
nIB
n}B
n�B
n�B
o B
oOB
oOB
o�B
o�B
o�B
p!B
p;B
pUB
poB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q�B
q�B
q�B
raB
r|B
r|B
r|B
r|B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u?B
uZB
utB
u�B
vB
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
v�B
wLB
wfB
w�B
w�B
w�B
xRB
xlB
xlB
xlB
x�B
x�B
x�B
xlB
xlB
x�B
y$B
yXB
yrB
yrB
y�B
y�B
z*B
z*B
zDB
zDB
z^B
z^B
z^B
z^B
z^B
z�B
z�B
z�B
z�B
{B
{0B
{JB
{B
{�B
{�B
{�B
|B
|B
|B
|PB
|�B
|�B
|�B
}<B
}qB
}qB
}�B
~(B
~wB
~]B
~]B
~wB
~wB
B
.B
HB
HB
HB
cB
�B
�B
� B
�iB
��B
��B
��B
��B
� B
�oB
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B�gB�gB�MB�MB��B��B�-B�B��B�uB�B��B�AB�[B�[B��B��B�MB�MB��B�B�#B�<B��B�+B��B��B��B��B��B	3B	OvB	�.B	��B	�+B	��B	�B
E�B
|�B
<B
VB
�@B
��B
��B3MBg�B��B��B��B�B^B(B�BB~BB iB�B��BޞB��B�@B�!B}qB^�BM�BA�B;BB�BE9B:�B$�B
�!B
�B
��B
XEB
#�B
�B
�B	�<B	��B	�mB	��B	�RB	��B	�B	�B	x�B	aHB	J�B	D�B	>B	4�B	)�B	$tB	 �B	�B	
�B��B�tB�B�B�6B�B�B�xBƨB��B��B��B��B��B��BĶB�PB�EB�B�B�B�&B�B�|B	B	&B	FB	�B	]B	&2B	(�B	0;B	6�B	@B	?�B	=B	=�B	=qB	>�B	>B	@4B	B�B	EB	F?B	H�B	LJB	X�B	j�B	oB	B	�KB	��B	��B	��B	�B	��B	�B	�#B	��B	�vB	��B	��B	��B	�FB	��B	��B	�`B	��B	�zB	��B	�eB	��B	��B	��B	��B	�[B	��B	��B	��B	��B	��B	�cB	�uB	�4B	�gB	��B	��B	��B	��B	ևB	�vB	�B	��B	�tB	��B	�-B	�B	��B	�B	��B	�bB	�NB	��B	��B	�B	�TB	��B	�NB	��B	�)B	��B	�CB	��B	�~B	ބB	�B	ܒB	��B	�hB	�\B	��B	ݲB	�,B	��B	߾B	ܬB	�-B	��B	�B	��B	�B	�B	�8B	�B	�B	�B	�B	�B	�B	��B	��B	�B	�:B	�4B	�-B	�bB	�-B	�4B	��B	��B	��B	�B	�B	��B	��B	�B	��B	�B	� B	�B	�hB	��B	��B	�B	�|B	�|B	�bB	��B	��B	�\B	��B	�~B	�WB	�B	��B	֡B	�2B	�2B	ԕB	�,B	��B	�[B	�[B	ӏB	�[B	�&B	�[B	�&B	��B	�B	�9B	�mB	�mB	ևB	�B	՛B	�gB	��B	�sB	�sB	�YB	�YB	�sB	��B	��B	׍B	׍B	׍B	ؓB	ٚB	��B	ٴB	�#B	��B	�)B	�]B	��B	�/B	ݘB	�B	�5B	�5B	�5B	�OB	޸B	��B	�!B	�VB	�pB	��B	�'B	��B	�HB	�B	�B	��B	�:B	�nB	�nB	�nB	��B	�B	�zB	�zB	�FB	�zB	�B	�$B	��B	��B	�_B	�yB	�B	�eB	�B	�B	�6B	�6B	�B	��B	��B	�"B	��B	�)B	��B	�B	��B	�cB	� B	��B	�OB	�;B	��B	��B	�B	�B	�|B	�aB	�B	�MB	�hB	�hB	�hB	�9B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�lB	��B	�$B	�*B	�*B	�B	��B	��B	��B	��B	�"B	�qB	�VB	�B	��B	��B	�wB	�]B	�wB	�BB	�(B	�BB	�HB
 B
 �B
UB
�B
oB
�B
[B
�B
�B
-B
{B
�B
�B
3B
3B
3B
B
�B
�B
mB
mB
�B
�B
�B
�B
	B
	�B
	�B
	7B
	RB
	�B

�B
�B
jB
6B
dB
B
�B
�B
dB
JB
0B
B
B
�B
~B
�B
~B
dB
PB
�B
�B
B
�B
�B
�B
�B
�B
�B
�B
B
.B
.B
bB
�B
B
4B
hB
�B
oB
�B
�B
�B
�B
uB
,B
�B
�B
�B
�B
B
mB
�B
�B
B
�B
7B
�B
�B
#B
WB
#B
=B
�B
�B
)B
]B
�B
�B
�B
B
VB
 B
 �B
 vB
 �B
!-B
!�B
"NB
"�B
"�B
"�B
"�B
#TB
#�B
#�B
#�B
$ZB
%,B
%zB
%�B
%�B
%�B
&�B
&�B
'B
(
B
(�B
(�B
(�B
(�B
)_B
)�B
*KB
*�B
*�B
*�B
+�B
+�B
,B
,B
,B
,B
,=B
,�B
,�B
-B
-wB
.IB
/B
0B
/�B
0;B
0oB
0!B
0oB
1AB
1�B
1�B
2-B
2GB
2B
2�B
4B
4nB
4TB
49B
4�B
4�B
5ZB
5�B
6FB
6�B
6�B
7LB
7�B
7�B
7�B
7�B
8B
8B
8�B
8�B
8�B
9$B
9XB
9�B
9�B
9�B
:B
:*B
:^B
:^B
:^B
:�B
;0B
;0B
;dB
;�B
;�B
<jB
<�B
<�B
="B
=qB
=qB
=�B
=�B
=�B
>BB
>�B
>�B
>�B
?B
?cB
?�B
?�B
@ B
@B
@4B
@4B
@iB
@�B
@�B
@�B
AUB
AUB
A�B
AoB
A�B
BB
BB
BuB
B�B
B�B
B�B
B�B
B�B
CaB
C�B
C�B
DMB
D�B
D�B
D�B
D�B
EB
EmB
EmB
F%B
F%B
E�B
FtB
F�B
F�B
F�B
F�B
GB
G+B
G�B
G�B
H1B
HKB
HfB
HfB
H�B
H�B
I7B
IB
I7B
I�B
I�B
J	B
J#B
J=B
J=B
J#B
JrB
J�B
J�B
KDB
K�B
LB
LB
K�B
K�B
LJB
LdB
L�B
MPB
M�B
N�B
N�B
OvB
OvB
O\B
OBB
O�B
O�B
PB
P.B
PHB
P�B
P�B
Q B
P�B
P�B
P�B
P}B
P}B
P�B
QB
QNB
QhB
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R�B
S@B
SuB
S�B
TB
TB
TB
T,B
T{B
T{B
T�B
UMB
U�B
VmB
W
B
W$B
W
B
W
B
W
B
W�B
W�B
W�B
XEB
XyB
X�B
X�B
X�B
YeB
YeB
YeB
Y�B
Y�B
ZB
ZB
Y�B
ZB
ZkB
Z�B
Z�B
Z�B
[qB
\B
\xB
\�B
\�B
\�B
]/B
]�B
]�B
^OB
^OB
^jB
^jB
^�B
^�B
^�B
_B
_;B
_�B
_�B
`'B
`�B
`�B
`�B
`�B
`�B
`�B
a-B
a|B
a�B
a�B
bB
bB
b4B
b4B
b4B
b4B
bhB
b�B
b�B
b�B
b�B
b�B
b�B
c B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
d�B
e,B
eFB
ezB
ezB
e�B
e�B
e�B
f2B
f2B
ffB
ffB
f�B
f�B
gB
g�B
h�B
i*B
i�B
jB
jB
j�B
j�B
j�B
k�B
l�B
mCB
m�B
m�B
nB
n/B
nIB
n}B
n�B
n�B
o B
oOB
oOB
o�B
o�B
o�B
p!B
p;B
pUB
poB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q�B
q�B
q�B
raB
r|B
r|B
r|B
r|B
r�B
r�B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u?B
uZB
utB
u�B
vB
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
v�B
wLB
wfB
w�B
w�B
w�B
xRB
xlB
xlB
xlB
x�B
x�B
x�B
xlB
xlB
x�B
y$B
yXB
yrB
yrB
y�B
y�B
z*B
z*B
zDB
zDB
z^B
z^B
z^B
z^B
z^B
z�B
z�B
z�B
z�B
{B
{0B
{JB
{B
{�B
{�B
{�B
|B
|B
|B
|PB
|�B
|�B
|�B
}<B
}qB
}qB
}�B
~(B
~wB
~]B
~]B
~wB
~wB
B
.B
HB
HB
HB
cB
�B
�B
� B
�iB
��B
��B
��B
��B
� B
�oB
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104936  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174515  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174515  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174515                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024522  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024522  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                