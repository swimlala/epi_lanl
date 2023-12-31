CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-07-20T00:35:18Z creation;2016-07-20T00:35:20Z conversion to V3.1;2019-12-19T08:31:25Z update;     
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
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �@   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �P   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �T   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �d   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �pArgo profile    3.1 1.2 19500101000000  20160720003518  20200116201517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_019                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @׼��o�1   @׼���-�@3�ߤ?��d���ߤ1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C]�fC_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��\@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B Q�B'�B/�B8Q�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B��\B��\B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]�HC_�HCa��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D'D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��)D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�)D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�|)D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D�D��\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�9XA��A�bA�A���A���A���A���A���A��A��AۓuA�p�A�jAײ-A�p�A�33A�A���A��
A֣�AփA�C�A�A՛�A��A�I�AэPA��HA��TA�?}A�t�Aʙ�AȃAǡ�A�A�t�A��A��yA��;A�ZA��A��A�t�A�Q�A�hsA�`BA�$�A��A���A�A���A��7A���A��7A��A��A��A��A���A��
A��9A��A��uA�9XA��-A�+A��PA�\)A�^5A���A�O�A�ĜA��;A��/A�^5A���A�oA�z�A���A��PA���A�+A���A�VA���A���A�=qA�l�A���A�ffA�VA�ZA��;A�|�A�=qA�  A�ƨA�A�A�"�A�C�A�ffA��wA��9A~�A|~�Az�9AyhsAx{AvĜAt$�Ar�Ap�AlE�Ai�TAhffAgC�Ad5?Ab~�Aa�wA_�;A]dZA\�9AX�AU��AS��AR  AP�/APE�AO�ANz�AL�HAK�AJ��AJ(�AHv�AGXAEdZADM�AC%AA&�A>�uA=A<A;/A: �A9+A8�A8�/A89XA5ƨA49XA3O�A2�uA/��A.$�A-?}A,$�A*�uA(=qA&z�A#�A"{A (�A33AQ�A��An�A�A33A�RAM�A��AK�A��A�hA�mA��A��A��A%A~�A1'AJA�^A��A��AO�A
=A��AZA �A�7A�RA-A��A`BA"�A
��A
�HA
�jA
�uA
-A	��AA&�A�AjA �AA��AZA$�A��A"�A�DAE�A�Al�A �D@��P@�$�@�V@��@�~�@��@�r�@���@�x�@�Z@�+@�E�@�  @�ƨ@�@�j@�P@�v�@��@��@�~�@�`B@���@�1'@��;@�o@���@��/@�A�@��
@ۅ@�C�@�+@��@ڗ�@�@�&�@�%@�I�@�1@׮@�-@�Q�@�ƨ@�C�@�33@��H@��@ѡ�@�&�@���@�r�@�9X@�ƨ@�\)@��@�~�@͙�@�Z@�
=@�=q@ɲ-@ɲ-@�x�@�I�@�C�@���@�&�@���@ă@�(�@î@�\)@�;d@�J@�&�@�G�@�O�@���@��@��T@�j@�dZ@�{@���@��@���@���@�%@�$�@�J@��T@���@��9@�bN@�Z@���@���@��@��@��\@�M�@�M�@�V@�J@���@�l�@��y@�@�o@���@�^5@�5?@��@��h@�Ĝ@��w@�{@��@��-@��-@�p�@��@� �@�t�@�dZ@�S�@�ȴ@���@�t�@�+@�;d@�ƨ@�
=@�+@�;d@�
=@�o@�ȴ@�V@�n�@���@�A�@�1'@���@�S�@���@��!@��#@�1'@�dZ@� �@��m@�K�@��@���@�M�@�hs@��`@�@�M�@�v�@�^5@�@��-@���@��h@��@���@�`B@�x�@��@�V@�1'@�|�@���@�  @��@� �@�;d@���@���@���@��!@�~�@���@���@���@�bN@�(�@��@���@��@��@�x�@���@�@�t�@�;d@�C�@��@�E�@�x�@���@��j@��u@�Z@�b@�ƨ@���@���@�M�@�=q@�$�@��!@��R@���@�%@���@���@��/@��/@��`@���@��@� �@��;@���@���@�^5@�V@�^5@�~�@�J@���@��@��/@��D@�Z@�9X@�1'@�(�@���@��P@�\)@�C�@�o@�ȴ@�E�@�$�@�$�@�J@��h@�hs@�G�@�7L@�/@�%@���@�Ĝ@��@���@��u@�z�@�Z@�1'@�1@��@��@�@���@��@��@�X@��@��`@��@��@�Z@�  @��w@��@�"�@���@�$�@��@���@���@�@���@���@�x�@�&�@�z�@�b@��
@���@�l�@�K�@��@���@���@���@��\@�n�@�E�@���@���@��@�I�@;d@~ff@}��@}p�@}?}@}V@|��@|1@{dZ@{C�@z��@z-@yX@x�u@xbN@w��@w|�@wK�@v�y@v�+@v@u`B@t�D@t�@sƨ@s��@s�@sS�@r�@r-@q�@q��@qX@p��@pA�@o�;@o\)@n��@n�R@nv�@m�@m�T@m��@m��@mO�@m�@mV@l�D@k�m@kt�@k@j�!@j�\@j^5@j-@i�@i�^@iX@h�@h  @g�;@g�w@g;d@g�@f�R@f{@e�@e@ep�@eV@d�j@dz�@d(�@c��@c��@cS�@c33@co@b~�@b-@a��@a�^@ax�@aX@a&�@`Ĝ@`�@_�@_��@_l�@_K�@_
=@^�y@^�R@^�+@^v�@^ff@^$�@]�h@]V@\�@[��@[�@[C�@[o@Z~�@Y�^@YG�@Y%@Xr�@W�@W\)@V$�@U@U�@U�@T��@T��@T��@TI�@T1@S�
@S�@SdZ@R�@RM�@Q��@Q��@Q��@Q7L@P��@P��@P�`@Q%@P��@PQ�@P �@O�@O\)@O�@N�R@N$�@N@M�@M/@L��@LI�@L(�@L1@K��@Ko@J��@JM�@I��@I��@Ihs@I&�@H��@H�u@HbN@HA�@H  @G�P@G\)@G
=@F�R@Fff@F5?@F@E@E�h@EO�@E?}@E�@D�j@DZ@D1@Cƨ@C��@Ct�@CS�@Co@Bn�@A�7@@��@@bN@@ �@@b@?�P@>��@>E�@=�T@=�-@=�h@=�@=p�@=?}@=/@=V@<��@<Z@<I�@<(�@;ƨ@;t�@:��@:n�@:-@9�7@9hs@9&�@8��@8Ĝ@8��@8�u@8Q�@7|�@7;d@7;d@7+@7
=@6ȴ@6�+@5�@5��@5�@5V@49X@3��@3�@3dZ@3"�@2M�@1�@1�@0r�@0bN@0bN@0Q�@0  @/��@/�P@/;d@.�@.V@.$�@-�T@-p�@-V@,��@,�@,j@,�@+��@+"�@*�\@*-@)��@)7L@)�@(��@(��@(Q�@'��@'l�@';d@&��@&��@&�+@&ff@&$�@%��@%�h@%p�@%p�@%p�@%?}@$��@$(�@#dZ@"�@"��@"��@"n�@"�@!�@!��@!&�@ �9@ �@ bN@ A�@  �@  �@ b@�;@�P@+@
=@��@��@��@��@ȴ@v�@V@@��@@�-@��@`B@�@(�@��@�m@�
@ƨ@�F@�F@�F@��@��@S�@�@�!@��@~�@=q@�#@��@hs@��@�@A�@�@��@�R@��@�+@v�@v�@V@V@E�@@�@?}@��@�@V@�@��@��@z�@j@j@Z@I�@9X@�@�m@�F@t�@S�@o@�@�H@�H@��@��@~�@^5@^5@M�@��@�7@X@G�@7L@�@�@�@��@Ĝ@�u@�@bN@A�@1'@A�@Q�@1'@ �@b@�@�;@�w@��@|�@|�@K�@
=@�@
=@��@�y@�@�@�@ȴ@�R@��@��@�+@ff@5?@�@�T@�T@@�-@�h@/@��@�/@�j@��@�D@z�@z�@z�@j@z�@(�@�m@ƨ@�F@��@��@�@C�@
��@
�!@
�!@
�!@
�!@
�\@
M�@
=q@
�@	��@	�@	�#@	�^@	��@	�7@	x�@	hs@	X@	&�@	%@��@�`@�u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�9XA��A�bA�A���A���A���A���A���A��A��AۓuA�p�A�jAײ-A�p�A�33A�A���A��
A֣�AփA�C�A�A՛�A��A�I�AэPA��HA��TA�?}A�t�Aʙ�AȃAǡ�A�A�t�A��A��yA��;A�ZA��A��A�t�A�Q�A�hsA�`BA�$�A��A���A�A���A��7A���A��7A��A��A��A��A���A��
A��9A��A��uA�9XA��-A�+A��PA�\)A�^5A���A�O�A�ĜA��;A��/A�^5A���A�oA�z�A���A��PA���A�+A���A�VA���A���A�=qA�l�A���A�ffA�VA�ZA��;A�|�A�=qA�  A�ƨA�A�A�"�A�C�A�ffA��wA��9A~�A|~�Az�9AyhsAx{AvĜAt$�Ar�Ap�AlE�Ai�TAhffAgC�Ad5?Ab~�Aa�wA_�;A]dZA\�9AX�AU��AS��AR  AP�/APE�AO�ANz�AL�HAK�AJ��AJ(�AHv�AGXAEdZADM�AC%AA&�A>�uA=A<A;/A: �A9+A8�A8�/A89XA5ƨA49XA3O�A2�uA/��A.$�A-?}A,$�A*�uA(=qA&z�A#�A"{A (�A33AQ�A��An�A�A33A�RAM�A��AK�A��A�hA�mA��A��A��A%A~�A1'AJA�^A��A��AO�A
=A��AZA �A�7A�RA-A��A`BA"�A
��A
�HA
�jA
�uA
-A	��AA&�A�AjA �AA��AZA$�A��A"�A�DAE�A�Al�A �D@��P@�$�@�V@��@�~�@��@�r�@���@�x�@�Z@�+@�E�@�  @�ƨ@�@�j@�P@�v�@��@��@�~�@�`B@���@�1'@��;@�o@���@��/@�A�@��
@ۅ@�C�@�+@��@ڗ�@�@�&�@�%@�I�@�1@׮@�-@�Q�@�ƨ@�C�@�33@��H@��@ѡ�@�&�@���@�r�@�9X@�ƨ@�\)@��@�~�@͙�@�Z@�
=@�=q@ɲ-@ɲ-@�x�@�I�@�C�@���@�&�@���@ă@�(�@î@�\)@�;d@�J@�&�@�G�@�O�@���@��@��T@�j@�dZ@�{@���@��@���@���@�%@�$�@�J@��T@���@��9@�bN@�Z@���@���@��@��@��\@�M�@�M�@�V@�J@���@�l�@��y@�@�o@���@�^5@�5?@��@��h@�Ĝ@��w@�{@��@��-@��-@�p�@��@� �@�t�@�dZ@�S�@�ȴ@���@�t�@�+@�;d@�ƨ@�
=@�+@�;d@�
=@�o@�ȴ@�V@�n�@���@�A�@�1'@���@�S�@���@��!@��#@�1'@�dZ@� �@��m@�K�@��@���@�M�@�hs@��`@�@�M�@�v�@�^5@�@��-@���@��h@��@���@�`B@�x�@��@�V@�1'@�|�@���@�  @��@� �@�;d@���@���@���@��!@�~�@���@���@���@�bN@�(�@��@���@��@��@�x�@���@�@�t�@�;d@�C�@��@�E�@�x�@���@��j@��u@�Z@�b@�ƨ@���@���@�M�@�=q@�$�@��!@��R@���@�%@���@���@��/@��/@��`@���@��@� �@��;@���@���@�^5@�V@�^5@�~�@�J@���@��@��/@��D@�Z@�9X@�1'@�(�@���@��P@�\)@�C�@�o@�ȴ@�E�@�$�@�$�@�J@��h@�hs@�G�@�7L@�/@�%@���@�Ĝ@��@���@��u@�z�@�Z@�1'@�1@��@��@�@���@��@��@�X@��@��`@��@��@�Z@�  @��w@��@�"�@���@�$�@��@���@���@�@���@���@�x�@�&�@�z�@�b@��
@���@�l�@�K�@��@���@���@���@��\@�n�@�E�@���@���@��@�I�@;d@~ff@}��@}p�@}?}@}V@|��@|1@{dZ@{C�@z��@z-@yX@x�u@xbN@w��@w|�@wK�@v�y@v�+@v@u`B@t�D@t�@sƨ@s��@s�@sS�@r�@r-@q�@q��@qX@p��@pA�@o�;@o\)@n��@n�R@nv�@m�@m�T@m��@m��@mO�@m�@mV@l�D@k�m@kt�@k@j�!@j�\@j^5@j-@i�@i�^@iX@h�@h  @g�;@g�w@g;d@g�@f�R@f{@e�@e@ep�@eV@d�j@dz�@d(�@c��@c��@cS�@c33@co@b~�@b-@a��@a�^@ax�@aX@a&�@`Ĝ@`�@_�@_��@_l�@_K�@_
=@^�y@^�R@^�+@^v�@^ff@^$�@]�h@]V@\�@[��@[�@[C�@[o@Z~�@Y�^@YG�@Y%@Xr�@W�@W\)@V$�@U@U�@U�@T��@T��@T��@TI�@T1@S�
@S�@SdZ@R�@RM�@Q��@Q��@Q��@Q7L@P��@P��@P�`@Q%@P��@PQ�@P �@O�@O\)@O�@N�R@N$�@N@M�@M/@L��@LI�@L(�@L1@K��@Ko@J��@JM�@I��@I��@Ihs@I&�@H��@H�u@HbN@HA�@H  @G�P@G\)@G
=@F�R@Fff@F5?@F@E@E�h@EO�@E?}@E�@D�j@DZ@D1@Cƨ@C��@Ct�@CS�@Co@Bn�@A�7@@��@@bN@@ �@@b@?�P@>��@>E�@=�T@=�-@=�h@=�@=p�@=?}@=/@=V@<��@<Z@<I�@<(�@;ƨ@;t�@:��@:n�@:-@9�7@9hs@9&�@8��@8Ĝ@8��@8�u@8Q�@7|�@7;d@7;d@7+@7
=@6ȴ@6�+@5�@5��@5�@5V@49X@3��@3�@3dZ@3"�@2M�@1�@1�@0r�@0bN@0bN@0Q�@0  @/��@/�P@/;d@.�@.V@.$�@-�T@-p�@-V@,��@,�@,j@,�@+��@+"�@*�\@*-@)��@)7L@)�@(��@(��@(Q�@'��@'l�@';d@&��@&��@&�+@&ff@&$�@%��@%�h@%p�@%p�@%p�@%?}@$��@$(�@#dZ@"�@"��@"��@"n�@"�@!�@!��@!&�@ �9@ �@ bN@ A�@  �@  �@ b@�;@�P@+@
=@��@��@��@��@ȴ@v�@V@@��@@�-@��@`B@�@(�@��@�m@�
@ƨ@�F@�F@�F@��@��@S�@�@�!@��@~�@=q@�#@��@hs@��@�@A�@�@��@�R@��@�+@v�@v�@V@V@E�@@�@?}@��@�@V@�@��@��@z�@j@j@Z@I�@9X@�@�m@�F@t�@S�@o@�@�H@�H@��@��@~�@^5@^5@M�@��@�7@X@G�@7L@�@�@�@��@Ĝ@�u@�@bN@A�@1'@A�@Q�@1'@ �@b@�@�;@�w@��@|�@|�@K�@
=@�@
=@��@�y@�@�@�@ȴ@�R@��@��@�+@ff@5?@�@�T@�T@@�-@�h@/@��@�/@�j@��@�D@z�@z�@z�@j@z�@(�@�m@ƨ@�F@��@��@�@C�@
��@
�!@
�!@
�!@
�!@
�\@
M�@
=q@
�@	��@	�@	�#@	�^@	��@	�7@	x�@	hs@	X@	&�@	%@��@�`@�u11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BM�BL�BL�BL�BL�BL�BM�BM�BM�BM�BL�B�qB)�B�BJBJBJBDBPBPBDBDBJBPBuB�B.B?}BS�BaHBo�B�1B�{B��B��B��B�B��B��B%BBJB�B�BVBVB �BPB��B�BȴBĜBB�wB�3B�qB��B��B��B��B��B�^BɺB�HB��B��B��B�B�B�`BBI�B��B-B�B\B%BB�B�BbB%B��B��B�B�fB��B��B��B�oB�\B�hB�Bt�BiyBJ�B�B
�B
�B
��B
��B
�FB
��B
�VB
|�B
iyB
YB
M�B
B�B
:^B
'�B
�B
\B	�B	�TB	�B	��B	ǮB	�LB	�B	��B	��B	��B	�+B	hsB	jB	cTB	^5B	ZB	W
B	R�B	G�B	B�B	;dB	6FB	0!B	)�B	"�B	�B	�B	PB	B��B��B�B�B�yB�sB�fB�ZB�)B��B��B��BƨB�wB�^B�LB�'B��B��B��B��B�{B�\B�DB�DB�=B�1B�1B�+B�%B�%B�%B�%B�B�B~�Bz�B|�Bv�Bu�Bv�B|�B�+B�{B��BĜBɺBɺBɺBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��BɺBȴBǮBȴBĜB�}B�qB�dB�}B�wB�jB�dB�XB�LB�LB�XB�^B�XB�RB�XB�XB�LB�LB�FB�FB�FB�?B�FB�FB�^B�qB�jB�dB�^B�^B�XB�RB�LB�LB�XB�qB�wB�}B��B��B��B��B��B��BBŢBĜBƨBƨBƨBǮB��B��B��B�B�/B�TB�ZB�`B�mB�sB�sB�sB�sB�mB�mB�fB�`B�yB�B��B��B	B	%B	
=B	VB	{B	�B	�B	�B	�B	�B	�B	#�B	%�B	)�B	,B	,B	)�B	&�B	"�B	&�B	(�B	,B	/B	49B	8RB	=qB	H�B	M�B	S�B	YB	YB	ZB	_;B	_;B	aHB	dZB	e`B	ffB	ffB	gmB	hsB	jB	iyB	ffB	dZB	jB	n�B	p�B	q�B	s�B	u�B	v�B	v�B	w�B	w�B	}�B	� B	�B	�%B	�%B	�1B	�7B	�7B	�=B	�PB	�JB	�7B	�DB	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�3B	�^B	�^B	�LB	�jB	ÖB	ÖB	��B	�jB	�RB	�jB	�wB	�wB	�qB	�wB	��B	�wB	�qB	�^B	�XB	�jB	�wB	�qB	�qB	�}B	��B	��B	B	ǮB	ȴB	ɺB	ȴB	ŢB	ĜB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�5B	�BB	�fB	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
  B
  B	��B	��B	��B
  B
B
B
B
B
B
  B	��B	��B	��B	��B	��B
B
B
B
  B	��B
  B
B
B
B
1B
1B
	7B

=B

=B

=B
DB
JB
DB
DB
JB
JB
PB
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
bB
bB
hB
uB
{B
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
�B
�B
�B
�B
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
!�B
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
!�B
!�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
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
&�B
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
)�B
+B
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
.B
.B
.B
.B
/B
/B
/B
/B
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
5?B
5?B
5?B
5?B
6FB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
9XB
9XB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
=qB
>wB
>wB
?}B
@�B
@�B
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
C�B
D�B
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
H�B
H�B
I�B
I�B
I�B
I�B
I�B
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
L�B
L�B
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
N�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
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
S�B
S�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
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
XB
XB
XB
XB
XB
XB
YB
YB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
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
`BB
`BB
`BB
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
dZB
dZB
dZB
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
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
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
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
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
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
z�B
{�B
z�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BNBL�BL�BL�BL�BL�BM�BM�BN"BN�BQB� B*�B�B�B�B�BxB�B�B�B�BB�B�BVB1�BB�BVBc�Bs�B�JB��B��B��B��B�B�LB�BB�B�BB)B4B�B$�B.B��B��BɆBŢBĶB��B��B��B�MB�B�<B��B��B�PBʦB�4B��B��B�+B��B�B�BʦBNVB��B.}BB B�B�BQB)BTBEB�.B��B�UB�B�?B�DB�dB��B��B�[B�gBv�Bm)BO�B�B
�2B
��B
�B
�3B
��B
��B
�NB
�B
k�B
Z�B
O�B
D�B
=<B
*0B
"B
�B	��B	�`B	�)B	�EB	ɠB	��B	��B	��B	��B	��B	��B	k6B	lWB	d�B	_!B	[=B	X�B	T�B	IB	C�B	<�B	8RB	1�B	,WB	$tB	�B	+B	HB	�B�6B��B��B�B��B��B��B�B�B�mBѝB��BȴB� B�6B��B�B��B�B�4B��B��B��B�~B��B�^B��B��B��B�B��B�EB��B�EB��B��B}�B�Bw�Bv+BwB}<B��B��B��B��B�=B�rB�=BʦB��B�~B�jB�vB�HB�.B�B�(B�BBϑB��B��B��B�^B�=B�7BȀB��B�SB� B�(B�6B�4B��B��B�jB��B�lB�lB�*B�0B�DB�	B�xB�xB�RB�8B�2B�LB�B��B��B�2B�JB�BB�<B��B�dB�B��B��B��B�B�DB�(B��B��B��B��B��B��B��B�'B�B��B�B��B�+B��B��B�4B�TB�MB�yB��B�B��B�B�B��B��B��B��B�
B�$B�RB�LB��B�B��B�cB	�B	�B	)B	�B	�B	�B	�B	B	B	 B	 �B	$ZB	%�B	*0B	,�B	-]B	*�B	'�B	#�B	'�B	)_B	,qB	/B	49B	8B	=B	H�B	N"B	TaB	Y�B	YB	ZkB	`'B	_VB	aHB	dtB	e�B	f�B	f�B	g�B	h�B	kQB	jeB	f�B	dtB	j�B	n�B	qB	q�B	s�B	vFB	w�B	w�B	x�B	xB	~(B	�B	�gB	��B	��B	��B	�RB	��B	��B	�pB	�B	��B	�DB	�6B	��B	��B	��B	��B	��B	�B	�B	��B	�XB	��B	��B	��B	�2B	��B	�3B	�MB	�uB	��B	�B	��B	��B	��B	��B	��B	�;B	�B	��B	��B	�XB	��B	��B	��B	��B	��B	��B	��B	�uB	ǮB	��B	�#B	�7B	�B	ĜB	ȚB	��B	�B	�pB	�(B	��B	� B	� B	�4B	�oB	�B	�NB	�4B	��B	ϑB	��B	��B	��B	�IB	�B	��B	��B	��B	��B	�B	�9B	�MB	��B	��B	��B	��B	��B	��B	��B	�;B	��B	��B	�B	��B	�B
 �B
 iB	�B	��B	��B
  B
B
;B
GB
aB
[B
 �B	�<B	�B	�B	��B	�B
[B
aB
oB
 4B	�.B
 4B
'B
'B
3B
fB
fB
	lB

XB

rB

�B
�B
dB
^B
xB
�B
dB
jB
jB
�B
pB
�B
vB
�B
vB
vB
vB
vB
�B
�B
�B
B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
5B
B
B
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
"NB
!HB
 B
B
B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
 B
 �B
 �B
 �B
 �B
 �B
!�B
"B
"�B
#B
#B
$B
$&B
%,B
%B
%�B
%�B
%�B
&B
&�B
'B
'B
'B
'B
'B
'B
($B
($B
)*B
)B
)B
)B
)B
)B
*0B
*KB
*KB
+6B
+B
+B
+6B
,"B
,=B
,=B
,=B
-)B
-)B
-CB
-CB
-)B
.IB
./B
./B
./B
/5B
/5B
/OB
/5B
/5B
0;B
0;B
0;B
0UB
0oB
1[B
1[B
2GB
2aB
2aB
2GB
2GB
2GB
2GB
2GB
2GB
2|B
3hB
3hB
4�B
4nB
4�B
5ZB
5ZB
5tB
5�B
6zB
7fB
7�B
7�B
7�B
7�B
8lB
8lB
9rB
9rB
9rB
9rB
9rB
:�B
:xB
:xB
:xB
:�B
;�B
;�B
<jB
<�B
=�B
>�B
>wB
?}B
@�B
@�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
EB
D�B
C�B
D�B
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
H�B
H�B
I�B
I�B
I�B
I�B
I�B
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
K�B
L�B
L�B
L�B
L�B
L�B
MB
MB
N"B
NB
MB
L�B
K�B
K�B
LB
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
MB
M�B
M�B
M�B
N"B
NB
OB
N�B
N�B
O(B
N�B
N�B
O�B
Q B
RB
SB
S@B
S@B
SB
R�B
S�B
T,B
TB
T,B
T,B
TB
T,B
U2B
UMB
U2B
VB
V9B
V9B
VmB
V9B
VSB
U2B
UB
T�B
VB
VB
VB
VB
VB
VSB
WYB
W$B
W$B
W?B
X+B
X+B
X+B
X+B
XEB
XEB
YKB
YKB
ZQB
ZQB
[=B
[=B
[=B
[=B
\]B
\]B
\]B
]dB
]dB
^OB
^jB
^OB
^OB
_pB
_VB
_VB
_;B
`BB
`\B
`vB
a|B
a�B
a|B
abB
abB
abB
bhB
bhB
bhB
b�B
cnB
cnB
cnB
c�B
dtB
dZB
d�B
dtB
dtB
d�B
e�B
f�B
ffB
ffB
f�B
f�B
f�B
gmB
h�B
h�B
hsB
hsB
h�B
h�B
h�B
i�B
j�B
j�B
jB
j�B
jB
jB
j�B
jB
kkB
j�B
k�B
k�B
l�B
m�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
o�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
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
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
zB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
zB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
|B
z�B
{�B
{B
|B
|B
}B
}B
|�B
}B
}B
}�B
}�B
}�B
}�B
}�B
~B
~(B
}�B
~(B
~B
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201607240038122016072400381220160724003812201806221259372018062212593720180622125937201804050658192018040506581920180405065819  JA  ARFMdecpA19c                                                                20160720093505  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160720003518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160720003519  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160720003519  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160720003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160720003520  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160720003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160720003520  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160720003520  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160720003520                      G�O�G�O�G�O�                JA  ARUP                                                                        20160720012022                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160720153703  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20160723153812  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160723153812  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404215819  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622035937  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201517                      G�O�G�O�G�O�                