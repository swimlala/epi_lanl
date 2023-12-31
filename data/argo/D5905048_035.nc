CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-09-06T18:37:38Z creation;2016-09-06T18:37:41Z conversion to V3.1;2019-12-19T08:27:24Z update;     
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
resolution        =���   axis      Z        t  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  `D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �@   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ܀   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20160906183738  20200116201517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               #A   JA  I2_0577_035                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @�ȵ���1   @�ȶ��O�@3-V�d��-�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+y�D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[fD[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�C311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @1�@~�R@�\)@�\)A�A?�A_�A�A��
A��
A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B�(�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C{C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CH{CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+xRD+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D3D3~�D3��D4~�D4��D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV�RDW~�DW��DX~�DX��DY~�DY��DZ~�D[D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�B�D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��\D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D���D��\D��\D�B�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�\)A�^5A�I�A�5?A�-A�1'A�1'A�1'A�+A�+A�+A�(�A�(�A�+A�+A�+A�+A�(�A�(�A�(�A�(�A�&�A�$�A�(�A�&�A�"�A�"�A��A�{A�
=A�%A�  A��A���A�bA���A��#A�v�A�A�A���A�M�A�t�A�\)A�S�A��A�ĜA���A�;dAϓuA��`Aͩ�A�VA��`A�ĜAʬA���A�oA���A��Aɲ-A�t�A�5?A���A�r�Aǝ�A��mAĕ�A��+A�l�A��A�dZA��A�-A��;A�5?A�$�A�~�A�/A��
A� �A�VA�`BA���A�ffA�  A��\A��\A���A�"�A��TA�A�A��jA��A��RA���A�x�A�A��A�Q�A���A�(�A���A�$�A��9A�\)A��uA�z�A���A���A��A��TA�ZA��TA�9XA��TA�JA�$�A���A��A�7LA~�A{�Ayl�Av�HAu`BAt�Ar�\ApJAnn�AnJAmK�Al�Ak
=Ai�Ah��Af��Ac�FAbI�Aa;dA^�/A]
=A[��AZ�`AX  AV�AU�AS��ARE�AQ��APVAO/AN1'AL�/AKO�AJ�AI��AG�AE�AD �AC�A@�A?;dA=�mA<�A;�FA8�A7
=A5�#A4�DA4�A3��A1�A1/A/ƨA-�A,��A,�`A,�`A,�/A,��A,1A)A(�RA&�yA&�A&r�A&A%��A%�A#�A#�A"��A"�uA!��A!G�A!"�A ��A E�A�7A��A�FAx�A�A�9Ar�A&�Al�AoA(�A�/A�FA��A��A�+A7LA(�A+A
bNA	�wA	/AJA"�A�uA�^A�`A�DAI�A�Ax�A�yAjA ��A bN@�|�@�J@��@��u@�  @�\)@�?}@��u@��;@���@�M�@��@���@�@�R@�@�G�@�?}@�ƨ@��@�hs@�K�@�ȴ@���@�F@�ƨ@��@�hs@�Q�@�V@��@�G�@���@��`@��;@�p�@� �@�n�@��@�r�@��@�v�@�@�&�@�b@�@�$�@؛�@ְ!@�J@�?}@�1@Ӯ@�t�@���@��@�x�@�O�@�dZ@��@���@�Ĝ@��
@�;d@ʟ�@ɺ^@�X@ȴ9@�  @�\)@�^5@�$�@�=q@ř�@���@ě�@ēu@�@�=q@��-@��u@��m@�K�@��!@�M�@�J@��@�G�@��@�bN@���@�"�@��H@�ȴ@�v�@�E�@�5?@�-@�V@�?}@��u@�9X@��m@��@�dZ@�+@��@��!@��@�&�@��/@�Ĝ@�z�@�I�@�1@�dZ@��y@���@��\@��@�V@�V@��@��@���@���@�K�@�l�@�V@��#@��^@�?}@�bN@�  @� �@��
@��@�33@��@���@�$�@��#@���@��@�p�@�7L@�A�@�b@��m@��@���@���@�M�@�n�@�M�@�V@�V@��T@��7@��@�Ĝ@���@�I�@���@�
=@���@���@���@���@���@�@�O�@�z�@��@�t�@�+@�
=@��@��H@��\@�J@���@�O�@�&�@��j@���@�Z@�j@���@��P@��y@�V@�^5@�^5@�=q@�{@���@��^@���@�V@���@��y@��H@��@�ȴ@���@��\@�~�@�v�@�V@�5?@��@�hs@�&�@��9@� �@���@���@�|�@�S�@�"�@��@��@���@��R@�ff@���@���@���@��7@��h@���@��h@���@���@�hs@��@��/@�bN@� �@��@��@�dZ@�+@���@�ȴ@��\@�n�@�M�@�{@���@��^@��-@��@�O�@��@�Ĝ@�b@�ƨ@��F@��P@�;d@��@��y@���@�v�@�E�@�$�@��#@�`B@�?}@�7L@�/@���@�bN@��F@�ƨ@�l�@��H@��+@�ff@�n�@�$�@��@��/@�bN@�Q�@�1'@��@��w@���@��P@�S�@���@��\@�-@���@��^@�x�@�O�@�X@�O�@��@���@��j@���@�j@�9X@���@���@��F@�t�@��@�ȴ@���@���@��\@�v�@�ff@�$�@���@���@���@�x�@�G�@�?}@�7L@�&�@��`@��9@��9@��@��@�bN@�(�@~��@~�R@~��@~V@}�@}/@|��@|(�@{�F@{dZ@{o@{@z�H@z�\@z-@y��@y�#@yG�@x��@x�9@xA�@w�w@v��@vff@v@u@t��@t�D@tZ@s�m@s��@sS�@s"�@so@r�\@r=q@q��@q�7@qX@q�@o��@o+@o�@o�@o+@o+@o+@o+@n��@nv�@mV@l�j@l�D@l�D@l�D@l�D@lz�@lj@lj@l9X@kƨ@ko@j��@jn�@j^5@j^5@j�@ix�@h��@h�@hA�@h �@h �@h  @g�@g|�@g\)@g+@f��@f��@e�T@eO�@d��@d��@dI�@c�m@cS�@c@b��@b�\@bM�@a��@a�@`��@`bN@_�@_�@_l�@_\)@_;d@^�y@^V@^$�@]�@]O�@\��@\j@\(�@[t�@[C�@["�@Z��@Z~�@ZM�@Y7L@X�`@X�9@XQ�@W�w@V��@VE�@V{@V@U�h@U�@T�j@T�@Tz�@T�@S33@R��@R^5@R-@Q��@Qx�@Q%@PQ�@O�@Ol�@OK�@O
=@N��@Nv�@Nff@M�@M�@L�@L�D@L9X@K�F@KC�@Ko@J�@J�@J�@J�@J�H@J�!@JM�@I��@I�^@Ix�@I%@H�`@H�@H �@Hb@G�;@G�@G\)@F�@FE�@E@Ep�@E�@D��@D�j@DI�@C�
@C�@CS�@CC�@Co@B��@B��@B-@A�@Ax�@@�9@@  @?��@?K�@?+@>�R@>�+@>V@=�@=�-@=�h@=?}@<��@<z�@<9X@;��@;dZ@;33@;o@;o@:��@:�\@:J@9��@9�@9��@9X@9�@8��@8�@8 �@8  @8  @7�;@7��@7�@7+@6��@6ff@6V@65?@5�T@5�@5�@5V@4�@4�D@3��@2�H@2��@2��@2^5@2J@1�@1X@0�`@0�u@0r�@0b@/|�@/;d@/+@/
=@.�@.ȴ@.��@.5?@-�T@-@-��@-?}@,�@,�D@,j@,I�@+��@+S�@*�H@*��@*��@*~�@*-@)�^@)�7@)hs@)X@)X@)G�@)7L@(Ĝ@(b@'�P@'\)@';d@&�y@&{@%�@%�T@%�h@$��@$��@$(�@#��@#�
@#ƨ@#��@#��@#S�@#o@#@#@"�@"�H@"��@"��@"^5@"�@!x�@!�@ ��@ �u@ r�@  �@�@�@+@��@��@��@v�@V@5?@$�@@?}@V@V@��@�/@�/@j@1@��@�@33@�H@��@��@�\@M�@-@J@�@�7@&�@�@��@��@Ĝ@�u@r�@r�@bN@1'@ �@�@��@�@l�@;d@
=@�+@$�@�-@p�@?}@V@��@�j@�@��@j@Z@Z@Z@(�@��@�
@�F@��@��@��@S�@33@"�@o@�@��@�\@=q@-@�@�@�@��@��@�7@X@�@�`@�@1'@�@�w@\)@�@�y@ȴ@v�@5?@$�@$�@�@�T@�-@p�@O�@?}@?}@/@��@�j@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�\)A�^5A�I�A�5?A�-A�1'A�1'A�1'A�+A�+A�+A�(�A�(�A�+A�+A�+A�+A�(�A�(�A�(�A�(�A�&�A�$�A�(�A�&�A�"�A�"�A��A�{A�
=A�%A�  A��A���A�bA���A��#A�v�A�A�A���A�M�A�t�A�\)A�S�A��A�ĜA���A�;dAϓuA��`Aͩ�A�VA��`A�ĜAʬA���A�oA���A��Aɲ-A�t�A�5?A���A�r�Aǝ�A��mAĕ�A��+A�l�A��A�dZA��A�-A��;A�5?A�$�A�~�A�/A��
A� �A�VA�`BA���A�ffA�  A��\A��\A���A�"�A��TA�A�A��jA��A��RA���A�x�A�A��A�Q�A���A�(�A���A�$�A��9A�\)A��uA�z�A���A���A��A��TA�ZA��TA�9XA��TA�JA�$�A���A��A�7LA~�A{�Ayl�Av�HAu`BAt�Ar�\ApJAnn�AnJAmK�Al�Ak
=Ai�Ah��Af��Ac�FAbI�Aa;dA^�/A]
=A[��AZ�`AX  AV�AU�AS��ARE�AQ��APVAO/AN1'AL�/AKO�AJ�AI��AG�AE�AD �AC�A@�A?;dA=�mA<�A;�FA8�A7
=A5�#A4�DA4�A3��A1�A1/A/ƨA-�A,��A,�`A,�`A,�/A,��A,1A)A(�RA&�yA&�A&r�A&A%��A%�A#�A#�A"��A"�uA!��A!G�A!"�A ��A E�A�7A��A�FAx�A�A�9Ar�A&�Al�AoA(�A�/A�FA��A��A�+A7LA(�A+A
bNA	�wA	/AJA"�A�uA�^A�`A�DAI�A�Ax�A�yAjA ��A bN@�|�@�J@��@��u@�  @�\)@�?}@��u@��;@���@�M�@��@���@�@�R@�@�G�@�?}@�ƨ@��@�hs@�K�@�ȴ@���@�F@�ƨ@��@�hs@�Q�@�V@��@�G�@���@��`@��;@�p�@� �@�n�@��@�r�@��@�v�@�@�&�@�b@�@�$�@؛�@ְ!@�J@�?}@�1@Ӯ@�t�@���@��@�x�@�O�@�dZ@��@���@�Ĝ@��
@�;d@ʟ�@ɺ^@�X@ȴ9@�  @�\)@�^5@�$�@�=q@ř�@���@ě�@ēu@�@�=q@��-@��u@��m@�K�@��!@�M�@�J@��@�G�@��@�bN@���@�"�@��H@�ȴ@�v�@�E�@�5?@�-@�V@�?}@��u@�9X@��m@��@�dZ@�+@��@��!@��@�&�@��/@�Ĝ@�z�@�I�@�1@�dZ@��y@���@��\@��@�V@�V@��@��@���@���@�K�@�l�@�V@��#@��^@�?}@�bN@�  @� �@��
@��@�33@��@���@�$�@��#@���@��@�p�@�7L@�A�@�b@��m@��@���@���@�M�@�n�@�M�@�V@�V@��T@��7@��@�Ĝ@���@�I�@���@�
=@���@���@���@���@���@�@�O�@�z�@��@�t�@�+@�
=@��@��H@��\@�J@���@�O�@�&�@��j@���@�Z@�j@���@��P@��y@�V@�^5@�^5@�=q@�{@���@��^@���@�V@���@��y@��H@��@�ȴ@���@��\@�~�@�v�@�V@�5?@��@�hs@�&�@��9@� �@���@���@�|�@�S�@�"�@��@��@���@��R@�ff@���@���@���@��7@��h@���@��h@���@���@�hs@��@��/@�bN@� �@��@��@�dZ@�+@���@�ȴ@��\@�n�@�M�@�{@���@��^@��-@��@�O�@��@�Ĝ@�b@�ƨ@��F@��P@�;d@��@��y@���@�v�@�E�@�$�@��#@�`B@�?}@�7L@�/@���@�bN@��F@�ƨ@�l�@��H@��+@�ff@�n�@�$�@��@��/@�bN@�Q�@�1'@��@��w@���@��P@�S�@���@��\@�-@���@��^@�x�@�O�@�X@�O�@��@���@��j@���@�j@�9X@���@���@��F@�t�@��@�ȴ@���@���@��\@�v�@�ff@�$�@���@���@���@�x�@�G�@�?}@�7L@�&�@��`@��9@��9@��@��@�bN@�(�@~��@~�R@~��@~V@}�@}/@|��@|(�@{�F@{dZ@{o@{@z�H@z�\@z-@y��@y�#@yG�@x��@x�9@xA�@w�w@v��@vff@v@u@t��@t�D@tZ@s�m@s��@sS�@s"�@so@r�\@r=q@q��@q�7@qX@q�@o��@o+@o�@o�@o+@o+@o+@o+@n��@nv�@mV@l�j@l�D@l�D@l�D@l�D@lz�@lj@lj@l9X@kƨ@ko@j��@jn�@j^5@j^5@j�@ix�@h��@h�@hA�@h �@h �@h  @g�@g|�@g\)@g+@f��@f��@e�T@eO�@d��@d��@dI�@c�m@cS�@c@b��@b�\@bM�@a��@a�@`��@`bN@_�@_�@_l�@_\)@_;d@^�y@^V@^$�@]�@]O�@\��@\j@\(�@[t�@[C�@["�@Z��@Z~�@ZM�@Y7L@X�`@X�9@XQ�@W�w@V��@VE�@V{@V@U�h@U�@T�j@T�@Tz�@T�@S33@R��@R^5@R-@Q��@Qx�@Q%@PQ�@O�@Ol�@OK�@O
=@N��@Nv�@Nff@M�@M�@L�@L�D@L9X@K�F@KC�@Ko@J�@J�@J�@J�@J�H@J�!@JM�@I��@I�^@Ix�@I%@H�`@H�@H �@Hb@G�;@G�@G\)@F�@FE�@E@Ep�@E�@D��@D�j@DI�@C�
@C�@CS�@CC�@Co@B��@B��@B-@A�@Ax�@@�9@@  @?��@?K�@?+@>�R@>�+@>V@=�@=�-@=�h@=?}@<��@<z�@<9X@;��@;dZ@;33@;o@;o@:��@:�\@:J@9��@9�@9��@9X@9�@8��@8�@8 �@8  @8  @7�;@7��@7�@7+@6��@6ff@6V@65?@5�T@5�@5�@5V@4�@4�D@3��@2�H@2��@2��@2^5@2J@1�@1X@0�`@0�u@0r�@0b@/|�@/;d@/+@/
=@.�@.ȴ@.��@.5?@-�T@-@-��@-?}@,�@,�D@,j@,I�@+��@+S�@*�H@*��@*��@*~�@*-@)�^@)�7@)hs@)X@)X@)G�@)7L@(Ĝ@(b@'�P@'\)@';d@&�y@&{@%�@%�T@%�h@$��@$��@$(�@#��@#�
@#ƨ@#��@#��@#S�@#o@#@#@"�@"�H@"��@"��@"^5@"�@!x�@!�@ ��@ �u@ r�@  �@�@�@+@��@��@��@v�@V@5?@$�@@?}@V@V@��@�/@�/@j@1@��@�@33@�H@��@��@�\@M�@-@J@�@�7@&�@�@��@��@Ĝ@�u@r�@r�@bN@1'@ �@�@��@�@l�@;d@
=@�+@$�@�-@p�@?}@V@��@�j@�@��@j@Z@Z@Z@(�@��@�
@�F@��@��@��@S�@33@"�@o@�@��@�\@=q@-@�@�@�@��@��@�7@X@�@�`@�@1'@�@�w@\)@�@�y@ȴ@v�@5?@$�@$�@�@�T@�-@p�@O�@?}@?}@/@��@�j@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�bB
�hB
�hB
�bB
�hB
�hB
�hB
�hB
�hB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�uB
�uB
�{B
��B
��B
�LB
��B
ɺB
��B
B
�-B
��B
��B
��B
��B
��B
��B
�dB
�qB
ȴB
ȴB
��B
�BbB/BH�BVBiyB�PB�!BɺB�mBB+BVBhB��B,B[#B�BB��B�hBȴB�B��B+BbBJBoB�BoBoBB��B��B�;B�#B�B��B��B�B��B�uBx�Bv�Bp�B\)BD�B33BB��B�XB�B��B��B�'B�B��B}�BcTBL�B2-B�BB
�TB
��B
�-B
��B
�PB
~�B
hsB
[#B
K�B
?}B
8RB
/B
�B
bB
DB
+B
B	��B	�B	�`B	�B	ǮB	�jB	�9B	��B	��B	�oB	�JB	}�B	p�B	iyB	aHB	YB	T�B	N�B	H�B	C�B	=qB	5?B	1'B	/B	$�B	�B	hB	JB	B��B�B�B�mB�#B��B��B��BɺBǮB��B�jB�FB�'B�B�B�B��B��B�B��B��B��B��B��B��B��B��B�uB�hB�bB�bB�\B�PB�JB�DB�7B�=B�1B�+B�7B�B�B�B�Bu�B{�B~�B|�B{�By�Bx�Bs�Bo�Bl�Bo�Bn�Bl�BjBe`BaHB`BB_;B_;B`BB`BBaHBbNBdZBdZBe`BcTBdZBdZBdZBcTBdZBdZBdZBcTBcTBcTBcTBdZBe`BdZBdZBhsBl�Bo�Bp�Bo�Bs�Bu�Bx�B��B��B��B��B�B�!B�-B�'B�'B�'B�'B�3B�9B�9B�3B�RB�dB��B��B��B��B��B��B��B��B��B�#B�#B�)B�#B�#B�)B�HB�NB�NB�sB�mB�sB�B�B�B��B��B��B��B	  B	B	B	B	bB	oB	{B	�B	�B	�B	!�B	%�B	(�B	-B	/B	/B	/B	1'B	2-B	5?B	5?B	9XB	=qB	@�B	@�B	A�B	E�B	F�B	G�B	G�B	O�B	S�B	Q�B	P�B	O�B	O�B	P�B	R�B	W
B	VB	T�B	T�B	T�B	W
B	XB	[#B	`BB	ffB	jB	iyB	iyB	iyB	m�B	o�B	q�B	r�B	s�B	r�B	t�B	w�B	x�B	~�B	�B	�B	�+B	�7B	�DB	�JB	�VB	�VB	�VB	�\B	�bB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�-B	�9B	�?B	�RB	�XB	�^B	�dB	�^B	�dB	�jB	�qB	��B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ŢB	ĜB	ĜB	ǮB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�5B	�ZB	�yB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
1B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
DB
DB
DB
JB
JB
PB
PB
VB
PB
VB
VB
VB
VB
\B
\B
\B
VB
\B
hB
hB
hB
bB
hB
hB
hB
bB
\B
\B
\B
VB
VB
VB
VB
VB
PB
JB
JB
PB
PB
PB
VB
VB
\B
bB
bB
bB
hB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
"�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
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
/B
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
5?B
5?B
6FB
6FB
6FB
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
9XB
9XB
9XB
:^B
:^B
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
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
@�B
@�B
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
C�B
C�B
C�B
D�B
C�B
C�B
C�B
C�B
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
G�B
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
H�B
H�B
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
L�B
K�B
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
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
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
YB
YB
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
[#B
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
cTB
cTB
cTB
cTB
cTB
cTB
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
hsB
gmB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
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
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
r�B
r�B
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
u�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�}B
��B
��B
�}B
�hB
�hB
�hB
��B
�hB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
�hB
��B
��B
��B
��B
��B
��B
��B
�IB
��B
��B
ΊB
�B
��B
��B
�+B
��B
�5B
��B
��B
�kB
��B
�"B
�4B
�lB
�#B
��B
�aBbB.�BH�BV�BkB�"B��B�rB�>B�B	�B�BB�HB.cB]B�YB�_B�TB�&B�=B��B �B	�BNB"BaB�B�BmB�B �B��B�BބB�]B��B�9B�vB��B��Bz�Bz*Bs�B^�BF�B6�B�BԕB��B��B�vB��B��B��B�ZB��BgBP}B6+B=BmB
�B
��B
��B
��B
��B
��B
j�B
^B
M�B
AUB
:xB
1�B
 \B
4B
JB
fB
�B	��B	�/B	�
B	�/B	ɠB	�BB	��B	�B	�dB	�aB	��B	�4B	r-B	kQB	b�B	Z7B	V�B	PbB	J=B	ESB	?HB	6�B	2�B	1�B	'RB	�B	@B	�B	3B��B�TB��B��B��BӏB�VB˒B��BɠB��B�]B�RB�-B�=B�B�6B��B��B��B��B��B�/B�)B�qB�QB��B�1B�B��B�B�NB�B��B��B�0B�XB��B�#B�	B�xB�?B��B��B��Bv�B}VB��B~wB}<B{dBz�ButBqBm�Bp�Bo�Bm�Bl"Bf�BbNBa|B`\B_�B`�B`�BbBc:Be�Bf2Bf2BdZBeFBeBd�Bc�BeBe�Bd�Bc�Bc�Bc�Bc�Be,BfLBeBeBh�Bl�Bp�Bq�BpUBt�Bu�BxB��B�sB�7B�$B�B�AB��B�vB�vB��B�-B��B�?B�ZB�B�$B�B�oB�VB�pBΥBϫB��B�B� BӏB��B��B�xB�qBۦB��B�B��B��B��B�B��B�IB�MB�3B�ZB�+B�XB�jB	 �B	�B	AB	SB	�B	B	�B	�B	�B	 \B	"hB	&�B	)yB	-�B	/�B	/iB	/OB	1[B	2�B	5�B	5�B	9�B	=�B	@�B	@�B	A�B	E�B	F�B	G�B	G�B	P�B	TaB	R:B	Q4B	PB	PB	QB	S@B	W?B	V�B	U�B	U2B	U2B	W?B	XEB	[qB	`�B	f�B	j�B	i�B	i�B	i�B	m�B	o�B	q�B	r�B	t�B	r�B	uB	xlB	y>B	.B	�oB	��B	�_B	�7B	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�XB	�eB	�KB	�CB	�CB	�cB	��B	�vB	�AB	�GB	�TB	�ZB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	ĶB	��B	��B	�(B	�4B	�HB	�:B	��B	�B	�B	�9B	�KB	�/B	�B	�@B	�DB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�8B	�	B	�>B	�>B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B
 B
AB
AB
UB
[B
AB
GB
GB
MB
gB
SB
YB
_B
KB
KB
fB
fB
	RB
	RB
	�B
	lB
	lB
	�B

�B
xB
^B
xB
xB
^B
~B
�B
�B
�B
�B
�B
�B
pB
pB
pB
�B
�B
�B
pB
�B
�B
�B
�B
}B
�B
�B
�B
�B
vB
�B
�B
�B
pB
pB
�B
�B
�B
�B
�B
jB
�B
jB
VB
pB
�B
}B
�B
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
	B
�B
�B
�B
B
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
"�B
#B
"�B
"�B
#B
$B
$&B
%B
%B
%B
%,B
%�B
$�B
&2B
'B
'B
'B
'B
'B
(
B
(
B
($B
($B
(>B
(sB
*0B
)�B
)�B
)�B
)�B
)�B
*0B
*B
*KB
*eB
,"B
,"B
,B
,B
,B
,B
,B
,"B
,"B
,=B
,=B
-)B
-)B
-)B
-)B
-]B
-CB
.IB
.IB
/5B
/5B
/B
/5B
/5B
/5B
/5B
0UB
0UB
0UB
1vB
1[B
1AB
1[B
2GB
2aB
2|B
3MB
3MB
3MB
3hB
3�B
4nB
4TB
4nB
4nB
5ZB
5ZB
5ZB
6`B
6zB
6zB
7fB
7fB
7�B
7�B
8�B
8lB
8�B
8lB
8lB
8�B
9rB
9�B
9�B
:xB
:xB
:�B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
@�B
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
C�B
C�B
C�B
D�B
C�B
C�B
C�B
C�B
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
G�B
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
H�B
H�B
I�B
I�B
I�B
I�B
K)B
KB
K�B
K�B
K�B
K�B
MB
K�B
L�B
L�B
L�B
L�B
M�B
NB
M�B
O(B
N�B
N�B
O�B
O�B
O�B
PB
PB
O�B
O�B
O�B
Q B
Q B
Q B
RB
RB
R B
Q�B
Q�B
RB
RB
R B
S&B
SB
S�B
TB
TB
TB
TB
UB
UB
U2B
UgB
V9B
W$B
W$B
W$B
W?B
W$B
WYB
XEB
X+B
X+B
XEB
YKB
Y1B
ZB
Z7B
ZB
Z7B
Z7B
ZkB
ZQB
[=B
[=B
[WB
[=B
[=B
[WB
\]B
\]B
]IB
]IB
]dB
^jB
^OB
^OB
_VB
_VB
_pB
_;B
_;B
_VB
_VB
_pB
_pB
_pB
_VB
_VB
`vB
`�B
abB
abB
a|B
a|B
a|B
bhB
bhB
bhB
bhB
cnB
cnB
cnB
cnB
cTB
cnB
cTB
cTB
c�B
cnB
cnB
cnB
d�B
d�B
ezB
e�B
ezB
e�B
ezB
ezB
e�B
f�B
ffB
ffB
f�B
f�B
g�B
g�B
g�B
g�B
g�B
gmB
gmB
g�B
h�B
g�B
h�B
h�B
i�B
i�B
i�B
i�B
i�B
j�B
i�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
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
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
q�B
r�B
r�B
r�B
r�B
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
u�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609100036532016091000365320160910003653201806221301482018062213014820180622130148201804050701032018040507010320180405070103  JA  ARFMdecpA19c                                                                20160907033506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160906183738  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160906183739  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160906183739  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160906183740  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160906183740  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160906183740  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160906183740  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160906183740  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160906183741                      G�O�G�O�G�O�                JA  ARUP                                                                        20160906192502                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160907154506  CV  JULD            G�O�G�O�F�E�                JM  ARCAJMQC2.0                                                                 20160909153653  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160909153653  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220103  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040148  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201517                      G�O�G�O�G�O�                