CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:40:03Z creation;2022-06-04T17:40:03Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604174003  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               aA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @٘
��6<1   @٘ 0��@/r-V�cAO�;dZ1   GPS     A   B   B   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp��BvffB��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B���B�  B�  B�  B�  C   C  C  C  CL�C
  C�fC�fC  C  C  C  C�fC  C�C�C   C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6�C8�C:  C<  C>�C?�fCA�fCC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd33Ce�fCg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@Q�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bp�RBvQ�B�B���B���B���B���B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B�(�B�\)B�B���B���B�B���B���B���B���B���C��C��C��CG�C	��C�GC�GC��C��C��C��C�GC��C{C{C��C!��C#��C%��C'�GC)��C+��C-��C/��C1��C3��C6{C8{C9��C;��C>{C?�GCA�GCC�GCE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Cb{Cd.Ce�GCg�GCi��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC�
>C�
>C��qC��qC��qC�
>C�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
>C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D �D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DXDX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�P�A�S�A�Z�A�[�A�\�A�YA�W?A�T�A�M�A�OBA�PA�O�A�NpA�E9A�F�A�7LA�_A��A���A���A��A���A���A��sA�бA��jA���A�ƨAש�A�3�A�HA�U�A���A�ΥA��rA�E�A̱�A�K�A���AʲaAɛ�A�-�A�v�A��A�r�A�^A��A���A���A�5A���A��yA�5A�S�A�D3A��A��]A�[WA�\�A�}�A��\A���A�[�A��gA���A�[�A�C�A��A�Q�A���A�+6A��MA��3A�ÖA��A���A�.A�NpA���A�9XA�g8A��A���A���A�tTA��A�!�A�n�A�,�A�&�A�T�A��_A�]dA���A}7�At�Ak�Ag� AbA`��A^�]A\ݘAW�~AS��AQ~ANC�ALs�AI��AH�AF�AAD�AB4�A@��A>�A;�bA:�A8($A4�\A2��A0یA0!-A/ںA-�<A+��A+(A*�zA+IRA+�
A+-A*�A*d�A(��A&��A%��A$��A%�A%�A&��A'��A'��A'C�A&�A%i�A$��A$�A#cA#B[A"��A"�A"c A"�A"ZA!��A!w�A �A4�A
�A��A��A��A�LA�A[�A�wAqvA1A�oA�jAf�A)�A/Ah�A�qA~A[�AAԕA�Aa|AG�AXyA�6A��AL0A�oATaA=�Av�A��A=�A��A�A
��A
�A<6A
��A
��A
{JA
�A
�A	��A	@A�KA>BA(�A� AU�A��A�jA�A�+A�A\�AeAݘAzA	A ��A �A w�A �@���A C�A x�A �3A �}A �6A ��A �MA 4@�&�@�F�@��X@�e�@�`B@�$�@�J�@�=q@��H@�e,@��@��@��@�zx@��s@��@�=q@�@��@���@��@�M@�j�@�!@�@�4@�#�@��@��@�1@��@�0�@�ߤ@�l�@��@�[@�o @�?}@�.@��@��.@���@��@��@��&@��@��@��@�c@�:�@�Z�@��@�w@�%F@���@�\�@��@��r@߬q@�iD@ޔF@�@�_@�e�@܇+@�a|@�}�@��@ڋD@�e�@��@ـ4@،@�@ל@��@֛�@�@�2a@Ԗ�@��@��f@�=q@��&@�^�@�C�@ρ@΃@�K�@��@��@�Ĝ@̀�@�@˴�@��@�G@ɹ�@ɫ�@ɲ-@Ɍ~@ɏ�@ɍP@�e,@�(@���@�Z�@��@��+@��@Ǘ�@�%F@��v@�(�@��@ű[@�iD@��c@Ĝx@�W�@��@Õ�@�[W@���@Y@�	@��-@�F�@��X@�q@��@��*@�c�@� i@���@��@�|�@��@���@�Z@�0U@�!@�  @���@��)@�2�@�|�@��@��@�r�@�@���@���@��{@���@�e@���@���@�`B@�@�`�@���@�zx@�	l@���@�oi@�H�@���@��2@�O@���@�K�@���@�~(@�6�@��)@��^@���@�=@�ѷ@��@�ی@��+@��5@���@�"h@�~�@�֡@��F@�;�@���@���@�#�@��.@�-@�ݘ@�n/@��m@�h�@�%�@���@�a�@��@���@� �@��N@���@�v`@�[W@�#�@��@���@�0U@��*@�=@�
=@�҉@��m@�N�@��7@�zx@�a�@�E9@��@�s�@�>B@��@��D@��^@�Vm@��@���@�I�@�#:@��j@���@�X�@�>�@�(@�}V@�9�@��@��K@��@���@�o�@�G�@�'�@��@���@��@��\@��@�_@�-@��@���@��f@�b�@�A @�+@��@�ߤ@���@�@�@�7@�� @��@��n@�_p@��2@���@�R�@��@���@�\�@��c@���@�n�@�GE@�>B@�($@�	@���@�8@�҉@���@�u�@���@���@�/@�Ɇ@��A@�1'@�	�@�ϫ@�c@�Vm@�)_@��@��I@�(�@���@���@���@�P�@�"�@��"@���@��@���@���@��@��d@���@���@��:@�^�@�1�@�S@��@��U@�|�@�N�@�5?@�	�@��Q@�J#@�ں@�8�@��@��@���@��@��p@���@���@�]d@�:*@��@~�H@~{�@~h
@~#:@}�@}G�@}&�@|�U@{F�@z�@zi�@y��@y�@x�|@x��@x�U@x�@xC-@w�q@w1�@vv�@v6�@u��@u[W@u�@tbN@tM@s�@s~�@sC�@r�"@rE�@q�Z@q�z@q��@q�@p��@pz�@p(�@o�@o��@o�f@n͟@np;@n�@m��@mO�@l�	@l�_@l$@k��@k�@kb�@j�X@j��@jv�@j�@i��@i8�@h��@hM@g��@go�@g4�@f�x@f�@e�X@d�v@d�o@d?�@c��@c�K@c�@cs@cMj@c8@b�R@b3�@a�=@aB�@a;@`��@`�u@`V�@`@_��@_.I@^��@^�}@^u%@^V@^�@]�N@]zx@]S&@\��@\Q�@\b@\�@[�@[{J@Z��@Z�@Y��@X��@X�?@XM@XG@W��@WJ#@V-@U��@U��@U��@Um]@U:�@T�E@T�@S�@Rv�@R�@Q��@Q!�@P��@P]d@P`�@P:�@O�@@Ot�@OP�@O@N��@N��@N?@M��@MB�@L��@L��@Lw�@K��@KF�@K6z@K)_@J�@J�@IA @H�@He�@G�Q@G��@G{J@G@F��@F{@E��@E�~@EA @E@D��@Dm�@DPH@C�a@C@O@B��@B�@B��@B��@Bh
@B
�@A�Z@A��@A�@@�f@@�@@�O@@y>@@Z@@(�@@�@?ݘ@?�@?~�@?dZ@?U�@?E9@?$t@>�H@>q�@>4@=�h@= \@<��@<'R@;�@;�*@;t�@:��@:q�@9��@9��@9zx@9+�@8�f@8��@8~(@8N�@8%�@8�@7��@7+@6�H@6�@6~�@6M�@5��@5Vm@4��@4�9@4Q�@44n@4@3�w@3|�@3j�@3Mj@3S@2^5@2�@1�.@1��@1p�@1`B@1J�@1@@0u�@/�r@/�a@/�{@/~�@/|�@/@O@.��@.Ta@.�@-��@-\�@,ی@,�?@,�O@,r�@,,=@+��@+�:@+o@*��@*��@*��@*W�@)�o@)�3@)��@)�h@)^�@)B�@(�v@(�@(9X@'�a@'8@',�@'Y@&�'@&Z�@&Ov@&�@%��@%�h@%��@%w2@%p�@%p�@%f�@%Dg@%q@%%@$�)@$�@$u�@$PH@$/�@#ƨ@#�P@#P�@#.I@#�@"�s@"��@"p;@!�>@!��@!��@!@@ �`@ �/@ �/@ ѷ@ �U@ ��@ �@ K^@ >B@�]@˒@�@9�@�@�X@��@{�@^5@6�@��@�@�@��@rG@B�@�@�$@z�@g8@[�@<�@6@�@�@�W@�:@&@@o@��@l�@)�@�@�d@��@*0@�Y@w�@g8@ �@ƨ@�4@g�@K�@'�@��@��@a|@�@�9@��@o @��@�)@��@y>@K^@�A@�6@�@,�@o@�@��@�]@�m@ff@V@Ov@J�@!�@�@��@m]@	l@�/@�z@A�@�}@X�@��@�x@h
@6�@@��@�^@zx@+�@@@%@�@�E@�p@�U@�z@PH@��@�k@dZ@/�@ i@
�@
��@
{�@
GE@
e@	��@	��@	�@	�d@	��@	Dg@�	@��@�D@Ft@*�@7@�r@ݘ@��@��@��@��@b�111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�P�A�S�A�Z�A�[�A�\�A�YA�W?A�T�A�M�A�OBA�PA�O�A�NpA�E9A�F�A�7LA�_A��A���A���A��A���A���A��sA�бA��jA���A�ƨAש�A�3�A�HA�U�A���A�ΥA��rA�E�A̱�A�K�A���AʲaAɛ�A�-�A�v�A��A�r�A�^A��A���A���A�5A���A��yA�5A�S�A�D3A��A��]A�[WA�\�A�}�A��\A���A�[�A��gA���A�[�A�C�A��A�Q�A���A�+6A��MA��3A�ÖA��A���A�.A�NpA���A�9XA�g8A��A���A���A�tTA��A�!�A�n�A�,�A�&�A�T�A��_A�]dA���A}7�At�Ak�Ag� AbA`��A^�]A\ݘAW�~AS��AQ~ANC�ALs�AI��AH�AF�AAD�AB4�A@��A>�A;�bA:�A8($A4�\A2��A0یA0!-A/ںA-�<A+��A+(A*�zA+IRA+�
A+-A*�A*d�A(��A&��A%��A$��A%�A%�A&��A'��A'��A'C�A&�A%i�A$��A$�A#cA#B[A"��A"�A"c A"�A"ZA!��A!w�A �A4�A
�A��A��A��A�LA�A[�A�wAqvA1A�oA�jAf�A)�A/Ah�A�qA~A[�AAԕA�Aa|AG�AXyA�6A��AL0A�oATaA=�Av�A��A=�A��A�A
��A
�A<6A
��A
��A
{JA
�A
�A	��A	@A�KA>BA(�A� AU�A��A�jA�A�+A�A\�AeAݘAzA	A ��A �A w�A �@���A C�A x�A �3A �}A �6A ��A �MA 4@�&�@�F�@��X@�e�@�`B@�$�@�J�@�=q@��H@�e,@��@��@��@�zx@��s@��@�=q@�@��@���@��@�M@�j�@�!@�@�4@�#�@��@��@�1@��@�0�@�ߤ@�l�@��@�[@�o @�?}@�.@��@��.@���@��@��@��&@��@��@��@�c@�:�@�Z�@��@�w@�%F@���@�\�@��@��r@߬q@�iD@ޔF@�@�_@�e�@܇+@�a|@�}�@��@ڋD@�e�@��@ـ4@،@�@ל@��@֛�@�@�2a@Ԗ�@��@��f@�=q@��&@�^�@�C�@ρ@΃@�K�@��@��@�Ĝ@̀�@�@˴�@��@�G@ɹ�@ɫ�@ɲ-@Ɍ~@ɏ�@ɍP@�e,@�(@���@�Z�@��@��+@��@Ǘ�@�%F@��v@�(�@��@ű[@�iD@��c@Ĝx@�W�@��@Õ�@�[W@���@Y@�	@��-@�F�@��X@�q@��@��*@�c�@� i@���@��@�|�@��@���@�Z@�0U@�!@�  @���@��)@�2�@�|�@��@��@�r�@�@���@���@��{@���@�e@���@���@�`B@�@�`�@���@�zx@�	l@���@�oi@�H�@���@��2@�O@���@�K�@���@�~(@�6�@��)@��^@���@�=@�ѷ@��@�ی@��+@��5@���@�"h@�~�@�֡@��F@�;�@���@���@�#�@��.@�-@�ݘ@�n/@��m@�h�@�%�@���@�a�@��@���@� �@��N@���@�v`@�[W@�#�@��@���@�0U@��*@�=@�
=@�҉@��m@�N�@��7@�zx@�a�@�E9@��@�s�@�>B@��@��D@��^@�Vm@��@���@�I�@�#:@��j@���@�X�@�>�@�(@�}V@�9�@��@��K@��@���@�o�@�G�@�'�@��@���@��@��\@��@�_@�-@��@���@��f@�b�@�A @�+@��@�ߤ@���@�@�@�7@�� @��@��n@�_p@��2@���@�R�@��@���@�\�@��c@���@�n�@�GE@�>B@�($@�	@���@�8@�҉@���@�u�@���@���@�/@�Ɇ@��A@�1'@�	�@�ϫ@�c@�Vm@�)_@��@��I@�(�@���@���@���@�P�@�"�@��"@���@��@���@���@��@��d@���@���@��:@�^�@�1�@�S@��@��U@�|�@�N�@�5?@�	�@��Q@�J#@�ں@�8�@��@��@���@��@��p@���@���@�]d@�:*@��@~�H@~{�@~h
@~#:@}�@}G�@}&�@|�U@{F�@z�@zi�@y��@y�@x�|@x��@x�U@x�@xC-@w�q@w1�@vv�@v6�@u��@u[W@u�@tbN@tM@s�@s~�@sC�@r�"@rE�@q�Z@q�z@q��@q�@p��@pz�@p(�@o�@o��@o�f@n͟@np;@n�@m��@mO�@l�	@l�_@l$@k��@k�@kb�@j�X@j��@jv�@j�@i��@i8�@h��@hM@g��@go�@g4�@f�x@f�@e�X@d�v@d�o@d?�@c��@c�K@c�@cs@cMj@c8@b�R@b3�@a�=@aB�@a;@`��@`�u@`V�@`@_��@_.I@^��@^�}@^u%@^V@^�@]�N@]zx@]S&@\��@\Q�@\b@\�@[�@[{J@Z��@Z�@Y��@X��@X�?@XM@XG@W��@WJ#@V-@U��@U��@U��@Um]@U:�@T�E@T�@S�@Rv�@R�@Q��@Q!�@P��@P]d@P`�@P:�@O�@@Ot�@OP�@O@N��@N��@N?@M��@MB�@L��@L��@Lw�@K��@KF�@K6z@K)_@J�@J�@IA @H�@He�@G�Q@G��@G{J@G@F��@F{@E��@E�~@EA @E@D��@Dm�@DPH@C�a@C@O@B��@B�@B��@B��@Bh
@B
�@A�Z@A��@A�@@�f@@�@@�O@@y>@@Z@@(�@@�@?ݘ@?�@?~�@?dZ@?U�@?E9@?$t@>�H@>q�@>4@=�h@= \@<��@<'R@;�@;�*@;t�@:��@:q�@9��@9��@9zx@9+�@8�f@8��@8~(@8N�@8%�@8�@7��@7+@6�H@6�@6~�@6M�@5��@5Vm@4��@4�9@4Q�@44n@4@3�w@3|�@3j�@3Mj@3S@2^5@2�@1�.@1��@1p�@1`B@1J�@1@@0u�@/�r@/�a@/�{@/~�@/|�@/@O@.��@.Ta@.�@-��@-\�@,ی@,�?@,�O@,r�@,,=@+��@+�:@+o@*��@*��@*��@*W�@)�o@)�3@)��@)�h@)^�@)B�@(�v@(�@(9X@'�a@'8@',�@'Y@&�'@&Z�@&Ov@&�@%��@%�h@%��@%w2@%p�@%p�@%f�@%Dg@%q@%%@$�)@$�@$u�@$PH@$/�@#ƨ@#�P@#P�@#.I@#�@"�s@"��@"p;@!�>@!��@!��@!@@ �`@ �/@ �/@ ѷ@ �U@ ��@ �@ K^@ >B@�]@˒@�@9�@�@�X@��@{�@^5@6�@��@�@�@��@rG@B�@�@�$@z�@g8@[�@<�@6@�@�@�W@�:@&@@o@��@l�@)�@�@�d@��@*0@�Y@w�@g8@ �@ƨ@�4@g�@K�@'�@��@��@a|@�@�9@��@o @��@�)@��@y>@K^@�A@�6@�@,�@o@�@��@�]@�m@ff@V@Ov@J�@!�@�@��@m]@	l@�/@�z@A�@�}@X�@��@�x@h
@6�@@��@�^@zx@+�@@@%@�@�E@�p@�U@�z@PH@��@�k@dZ@/�@ i@
�@
��@
{�@
GE@
e@	��@	��@	�@	�d@	��@	Dg@�	@��@�D@Ft@*�@7@�r@ݘ@��@��@��@��@b�111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
�6B
��B
�B
��B
�B
�WB
��B
��B
��B
��B
��B
��B
�B
��B
�)B
��B
�CB
��B
��B
��B
�qB
�"B
��B
�KB
�eB
�KB
�B
��B
��B
�B
�?B
��B
��B
�B
��B
�B
��B
��B
��B
��B
�B
ΥB
�}B
�gB�B�B)BgB��B�MB��B{�B�'B�NB�B��B�3B�
BB�BoBxB~B�B�B�BB��B�B�xB��B��B��B��B�B|PB_�BI�BA�B8�B%FBvB�B
��B
ѝB
��B
�B
|jB
m]B
\�B
D�B
2-B
%`B
	RB	өB	��B	�B	dtB	W�B	M�B	?}B	+QB	�B		�B��B�`B�cB�wB�qB��B�>B�HB��B��B��B�qB	�B	tB	EB	�B		�B	�B	 B	�B	1�B	T�B	sMB	��B	��B	�hB	�=B	�aB	��B	��B	��B	B	خB	�vB	�B	��B	�aB	�CB	�kB	��B	�B	�B	�B	��B	�B
	B
	�B
KB
	B
	�B
 4B	��B	�oB	�B	�B	�5B	��B	��B	�rB	�B	�`B	��B	��B	�fB	��B	�B	�OB	�yB	��B	�8B	�fB	�zB	�&B	�B	��B	�B	��B	�B	�:B	�B	��B	�fB	��B	��B	�VB	�dB	�/B	�B	�B	��B	�!B	��B	��B	��B	�nB	�B	�oB	��B	�KB	��B	�mB	�FB	�hB	��B	چB	ٴB	چB	�+B	�YB	�mB	�9B	ևB	�B	��B	�QB	��B	��B	�LB	��B	��B	�`B	�rB	�JB
 iB
B	��B	�wB
�B
B
 �B	�wB	��B	��B	�%B	�?B	��B	��B	�ZB	��B	��B	��B	�B	�B	�?B	�nB	�B	�B	�;B	�B	��B	�AB	�!B	�B	�IB	�)B	�qB	��B	��B	�0B	�B	�6B	��B	��B	�kB	�B	�DB	��B	��B	��B	��B	�B	�9B	�FB	�RB	�XB	�xB	��B	��B	��B	��B	�B	�tB	��B	�%B	�9B	�hB	�nB	�B	�9B	�`B	��B	�fB	��B	��B	�B	�lB	�fB	��B	��B	��B	�	B	�$B	�B	��B	�B	�B	�6B	��B	�xB	�DB	�rB	��B	��B	�B	��B	��B	��B	�+B	�B	�B	�B	�8B	�$B	�B	��B	�}B
 OB
 �B
B
 B
�B
AB
�B
�B
�B
B
B
GB
�B
�B
uB
�B
�B
uB
'B
uB
�B
UB
 B
UB
 B
B
 �B
 OB
  B
  B	��B	��B	�}B	��B	��B	��B	�HB	�}B	�HB	�HB	�B	��B	��B	��B	�]B	�qB	�BB	�B	��B	�cB	�}B	�cB	�.B
 iB
 OB
 �B
 �B
 �B
 �B
UB
UB
B
�B
�B
GB
�B
�B
[B
 B
 �B
 iB	��B	�.B	��B	��B	��B	��B	��B	�qB
 4B
 4B	�HB	��B	��B	��B	�^B	��B	�PB	�}B	��B	��B	�B	��B	��B	�]B	�(B	��B	��B	��B	�B	�(B	�B	��B
  B
 OB
 OB
 �B
 �B
 �B
 �B
�B
�B
�B
B
aB
GB
�B
�B
gB
gB
�B
�B
mB
B
?B
?B
�B
�B
�B
�B
�B
�B
�B
	�B

XB

�B
^B
�B
�B
�B
^B

XB

�B
�B
�B
�B
hB
�B
[B
�B
�B
2B
�B
B
�B
=B
xB
�B
�B
�B
B
dB
�B
5B
B
�B
�B
�B
�B
VB
�B
�B
 B
 �B
 vB
!-B
!HB
!bB
!bB
!bB
!bB
!HB
!�B
"�B
# B
"�B
"�B
$&B
$ZB
$�B
$�B
$tB
$@B
$tB
$�B
$�B
$�B
%,B
%`B
%�B
&�B
&�B
&fB
&�B
&�B
'B
&�B
'8B
'RB
'8B
'B
'�B
(
B
(
B
($B
($B
(�B
(�B
)*B
)_B
*eB
+�B
+�B
,B
,=B
,=B
,�B
-)B
./B
.B
.IB
.�B
0;B
0!B
0B
/�B
/�B
/iB
/�B
1AB
2B
2-B
2�B
2�B
2�B
2�B
3B
4nB
4�B
4�B
5tB
5�B
5�B
5�B
5�B
5�B
6+B
6`B
6�B
6�B
7B
7LB
7�B
8B
8lB
8�B
8�B
8�B
8�B
9	B
9�B
:B
:^B
:^B
:xB
:�B
;dB
;�B
;�B
;�B
;�B
<PB
<PB
<�B
<�B
="B
=<B
=�B
=�B
=�B
>B
=�B
>�B
>�B
>�B
>�B
?HB
?cB
?cB
?�B
@ B
@ B
?�B
@OB
@OB
@4B
@�B
@iB
@OB
@�B
@�B
A;B
AoB
AoB
AUB
A�B
A�B
A�B
B'B
B�B
B�B
B�B
B�B
C-B
C�B
C�B
C�B
D3B
DMB
D�B
D�B
D�B
EB
D�B
E�B
E�B
E�B
FB
E�B
FYB
F�B
F�B
GEB
F�B
G_B
GzB
GzB
G�B
HB
I�B
I�B
IlB
IRB
I�B
I�B
I�B
J	B
J=B
I�B
I�B
J	B
J=B
K^B
KDB
K)B
J�B
J�B
J�B
J�B
KDB
KDB
K)B
KB
J�B
J	B
I�B
IlB
IB
I�B
I�B
I�B
I�B
I�B
J�B
KxB
K�B
LB
L�B
L�B
L�B
L�B
M6B
M�B
M�B
M�B
NB
N"B
NpB
N�B
N�B
OBB
O�B
O�B
O�B
PB
PHB
P}B
P�B
P�B
QNB
Q�B
Q�B
Q�B
R B
RoB
R�B
S�B
S�B
TB
T,B
TaB
T�B
T�B
T�B
T�B
T�B
U2B
U�B
VB
V9B
V�B
V9B
V9B
VB
V�B
W�B
W�B
XEB
XEB
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Z�B
[WB
[WB
[qB
[�B
[�B
\B
\)B
\CB
\�B
\�B
\�B
]dB
]�B
]�B
]�B
]�B
]�B
]�B
]�B
_!B
_VB
_VB
_VB
_�B
`'B
_�B
_�B
`\B
`vB
`BB
`�B
`�B
a-B
aHB
a|B
a�B
b�B
b�B
cB
c:B
c�B
c�B
c�B
d@B
dZB
dtB
dtB
d�B
e`B
e`B
ezB
e�B
ezB
e�B
ffB
f�B
f�B
gRB
g�B
g�B
h>B
h�B
i_B
i*B
iDB
i�B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
jB
j0B
j�B
kB
kB
kQB
kkB
lB
lWB
lqB
l�B
l�B
l�B
l�B
mB
m�B
m�B
m�B
nIB
n/B
nIB
nIB
ncB
ncB
n}B
n�B
n�B
n�B
o5B
o5B
o5B
o�B
o�B
pB
p!B
p;B
p;B
pUB
p�B
p�B
p�B
p�B
qB
q[B
qvB
q�B
rGB
rGB
rGB
r|B
raB
r�B
r�B
r�B
s3B
shB
sMB
sMB
s�B
t9B
t9B
tnB
t�B
t�B
u?B
vB
u�B
u�B
vFB
v�B
v�B
v�B
v�B
wB
wLB
w�B
w�B
xB
xB
xlB
x�B
y	B
yXB
yXB
yrB
y�B
y�B
y�B
z^B
z�B
z�B
z�B
z�B
z�B
z�B
{dB
{dB
{dB
{dB
{�B
{�B
|B
|B
|�B
|�B
|�B
}"B
}�B
~B
~]B
~�B
~�B
B
.B
.B
}B
�B
�4B
�OB
�iB
��B
��B
��B
��B
��B
�B
�oB
��B
��B
��B
�'B
�[B
�uB
��B
�B
�GB
�GB
�aB
�aB
�aB
��B
�B
�gB
��B
��B
�B
�B
�B
�9B
�SB
��B
��B
��B
��B
��111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
��B
�6B
��B
�B
��B
�B
�WB
��B
��B
��B
��B
��B
��B
�B
��B
�)B
��B
�CB
��B
��B
��B
�qB
�"B
��B
�KB
�eB
�KB
�B
��B
��B
�B
�?B
��B
��B
�B
��B
�B
��B
��B
��B
��B
�B
ΥB
�}B
�gB�B�B)BgB��B�MB��B{�B�'B�NB�B��B�3B�
BB�BoBxB~B�B�B�BB��B�B�xB��B��B��B��B�B|PB_�BI�BA�B8�B%FBvB�B
��B
ѝB
��B
�B
|jB
m]B
\�B
D�B
2-B
%`B
	RB	өB	��B	�B	dtB	W�B	M�B	?}B	+QB	�B		�B��B�`B�cB�wB�qB��B�>B�HB��B��B��B�qB	�B	tB	EB	�B		�B	�B	 B	�B	1�B	T�B	sMB	��B	��B	�hB	�=B	�aB	��B	��B	��B	B	خB	�vB	�B	��B	�aB	�CB	�kB	��B	�B	�B	�B	��B	�B
	B
	�B
KB
	B
	�B
 4B	��B	�oB	�B	�B	�5B	��B	��B	�rB	�B	�`B	��B	��B	�fB	��B	�B	�OB	�yB	��B	�8B	�fB	�zB	�&B	�B	��B	�B	��B	�B	�:B	�B	��B	�fB	��B	��B	�VB	�dB	�/B	�B	�B	��B	�!B	��B	��B	��B	�nB	�B	�oB	��B	�KB	��B	�mB	�FB	�hB	��B	چB	ٴB	چB	�+B	�YB	�mB	�9B	ևB	�B	��B	�QB	��B	��B	�LB	��B	��B	�`B	�rB	�JB
 iB
B	��B	�wB
�B
B
 �B	�wB	��B	��B	�%B	�?B	��B	��B	�ZB	��B	��B	��B	�B	�B	�?B	�nB	�B	�B	�;B	�B	��B	�AB	�!B	�B	�IB	�)B	�qB	��B	��B	�0B	�B	�6B	��B	��B	�kB	�B	�DB	��B	��B	��B	��B	�B	�9B	�FB	�RB	�XB	�xB	��B	��B	��B	��B	�B	�tB	��B	�%B	�9B	�hB	�nB	�B	�9B	�`B	��B	�fB	��B	��B	�B	�lB	�fB	��B	��B	��B	�	B	�$B	�B	��B	�B	�B	�6B	��B	�xB	�DB	�rB	��B	��B	�B	��B	��B	��B	�+B	�B	�B	�B	�8B	�$B	�B	��B	�}B
 OB
 �B
B
 B
�B
AB
�B
�B
�B
B
B
GB
�B
�B
uB
�B
�B
uB
'B
uB
�B
UB
 B
UB
 B
B
 �B
 OB
  B
  B	��B	��B	�}B	��B	��B	��B	�HB	�}B	�HB	�HB	�B	��B	��B	��B	�]B	�qB	�BB	�B	��B	�cB	�}B	�cB	�.B
 iB
 OB
 �B
 �B
 �B
 �B
UB
UB
B
�B
�B
GB
�B
�B
[B
 B
 �B
 iB	��B	�.B	��B	��B	��B	��B	��B	�qB
 4B
 4B	�HB	��B	��B	��B	�^B	��B	�PB	�}B	��B	��B	�B	��B	��B	�]B	�(B	��B	��B	��B	�B	�(B	�B	��B
  B
 OB
 OB
 �B
 �B
 �B
 �B
�B
�B
�B
B
aB
GB
�B
�B
gB
gB
�B
�B
mB
B
?B
?B
�B
�B
�B
�B
�B
�B
�B
	�B

XB

�B
^B
�B
�B
�B
^B

XB

�B
�B
�B
�B
hB
�B
[B
�B
�B
2B
�B
B
�B
=B
xB
�B
�B
�B
B
dB
�B
5B
B
�B
�B
�B
�B
VB
�B
�B
 B
 �B
 vB
!-B
!HB
!bB
!bB
!bB
!bB
!HB
!�B
"�B
# B
"�B
"�B
$&B
$ZB
$�B
$�B
$tB
$@B
$tB
$�B
$�B
$�B
%,B
%`B
%�B
&�B
&�B
&fB
&�B
&�B
'B
&�B
'8B
'RB
'8B
'B
'�B
(
B
(
B
($B
($B
(�B
(�B
)*B
)_B
*eB
+�B
+�B
,B
,=B
,=B
,�B
-)B
./B
.B
.IB
.�B
0;B
0!B
0B
/�B
/�B
/iB
/�B
1AB
2B
2-B
2�B
2�B
2�B
2�B
3B
4nB
4�B
4�B
5tB
5�B
5�B
5�B
5�B
5�B
6+B
6`B
6�B
6�B
7B
7LB
7�B
8B
8lB
8�B
8�B
8�B
8�B
9	B
9�B
:B
:^B
:^B
:xB
:�B
;dB
;�B
;�B
;�B
;�B
<PB
<PB
<�B
<�B
="B
=<B
=�B
=�B
=�B
>B
=�B
>�B
>�B
>�B
>�B
?HB
?cB
?cB
?�B
@ B
@ B
?�B
@OB
@OB
@4B
@�B
@iB
@OB
@�B
@�B
A;B
AoB
AoB
AUB
A�B
A�B
A�B
B'B
B�B
B�B
B�B
B�B
C-B
C�B
C�B
C�B
D3B
DMB
D�B
D�B
D�B
EB
D�B
E�B
E�B
E�B
FB
E�B
FYB
F�B
F�B
GEB
F�B
G_B
GzB
GzB
G�B
HB
I�B
I�B
IlB
IRB
I�B
I�B
I�B
J	B
J=B
I�B
I�B
J	B
J=B
K^B
KDB
K)B
J�B
J�B
J�B
J�B
KDB
KDB
K)B
KB
J�B
J	B
I�B
IlB
IB
I�B
I�B
I�B
I�B
I�B
J�B
KxB
K�B
LB
L�B
L�B
L�B
L�B
M6B
M�B
M�B
M�B
NB
N"B
NpB
N�B
N�B
OBB
O�B
O�B
O�B
PB
PHB
P}B
P�B
P�B
QNB
Q�B
Q�B
Q�B
R B
RoB
R�B
S�B
S�B
TB
T,B
TaB
T�B
T�B
T�B
T�B
T�B
U2B
U�B
VB
V9B
V�B
V9B
V9B
VB
V�B
W�B
W�B
XEB
XEB
X�B
X�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Z�B
[WB
[WB
[qB
[�B
[�B
\B
\)B
\CB
\�B
\�B
\�B
]dB
]�B
]�B
]�B
]�B
]�B
]�B
]�B
_!B
_VB
_VB
_VB
_�B
`'B
_�B
_�B
`\B
`vB
`BB
`�B
`�B
a-B
aHB
a|B
a�B
b�B
b�B
cB
c:B
c�B
c�B
c�B
d@B
dZB
dtB
dtB
d�B
e`B
e`B
ezB
e�B
ezB
e�B
ffB
f�B
f�B
gRB
g�B
g�B
h>B
h�B
i_B
i*B
iDB
i�B
iyB
i�B
i�B
i�B
i�B
i�B
i�B
jB
j0B
j�B
kB
kB
kQB
kkB
lB
lWB
lqB
l�B
l�B
l�B
l�B
mB
m�B
m�B
m�B
nIB
n/B
nIB
nIB
ncB
ncB
n}B
n�B
n�B
n�B
o5B
o5B
o5B
o�B
o�B
pB
p!B
p;B
p;B
pUB
p�B
p�B
p�B
p�B
qB
q[B
qvB
q�B
rGB
rGB
rGB
r|B
raB
r�B
r�B
r�B
s3B
shB
sMB
sMB
s�B
t9B
t9B
tnB
t�B
t�B
u?B
vB
u�B
u�B
vFB
v�B
v�B
v�B
v�B
wB
wLB
w�B
w�B
xB
xB
xlB
x�B
y	B
yXB
yXB
yrB
y�B
y�B
y�B
z^B
z�B
z�B
z�B
z�B
z�B
z�B
{dB
{dB
{dB
{dB
{�B
{�B
|B
|B
|�B
|�B
|�B
}"B
}�B
~B
~]B
~�B
~�B
B
.B
.B
}B
�B
�4B
�OB
�iB
��B
��B
��B
��B
��B
�B
�oB
��B
��B
��B
�'B
�[B
�uB
��B
�B
�GB
�GB
�aB
�aB
�aB
��B
�B
�gB
��B
��B
�B
�B
�B
�9B
�SB
��B
��B
��B
��B
��111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104923  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174003  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174003  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174003                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024011  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024011  QCF$                G�O�G�O�G�O�            4000JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                