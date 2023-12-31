CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-27T00:36:33Z creation;2019-01-27T00:36:39Z conversion to V3.1;2019-12-19T07:22:27Z update;     
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
_FillValue                  �  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �@   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �P   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190127003633  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              EA   JA  I2_0576_325                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @آ�~�/ 1   @آ�}'Ҁ@9�K]�c��dS��u�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��fD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��\@�\)@�\)A�A?�A_�A�A��
A�
=A��
A��
A��
A��
A��
A��
B�B�B�B�B'�B/�B7�B?�BG�BO�BW�B_�Bg�Bo�Bw�B�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;�HC=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��qC��qC��qC��qC��qC��qC�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��C��qC��qC��qC��qC��qC��qC��qC�
=C��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qC��qD ~�D ��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D	~�D	��D
~�D
��D~�D��D~�D��D~�D��DxRD��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D~�D��D ~�D ��D!~�D!��D"~�D"��D#~�D#��D$~�D$��D%~�D%��D&~�D&��D'~�D'��D(~�D(��D)~�D)��D*~�D*��D+~�D+��D,~�D,��D-~�D-��D.~�D.��D/~�D/��D0~�D0��D1~�D1��D2~�D2��D3~�D3��D4~�D5D5~�D5��D6~�D6��D7~�D7��D8~�D8��D9~�D9��D:~�D:��D;~�D;��D<~�D<��D=~�D=��D>~�D>��D?~�D?��D@~�D@��DA~�DA��DB~�DB��DC~�DC��DD~�DD��DE~�DE��DF~�DF��DG~�DG��DH~�DH��DI~�DI��DJ~�DJ��DK~�DK��DL~�DL��DM~�DM��DN~�DN��DO~�DO��DP~�DP��DQ~�DQ��DR~�DR��DS~�DS��DT~�DT��DU~�DU��DV~�DV��DW~�DW��DX~�DX��DY~�DY��DZ~�DZ��D[~�D[��D\~�D\��D]~�D]��D^~�D^��D_~�D_��D`~�D`��Da~�Da��Db~�Db��Dc~�Dc��Dd~�Dd��De~�De��Df~�Df��Dg~�Dg��Dh~�Dh��Di~�Di��Dj~�Dj��Dk~�Dk��Dl~�Dl��Dm~�Dm��Dn~�Dn��Do~�Do��Dp~�Dp��Dq~�Dq��Dr~�Dr��Ds~�Ds��Dt~�Dt��Du~�Du��Dv~�Dv��Dw~�Dw��Dx~�Dx��Dy~�Dy��Dz~�Dz��D{~�D{��D|~�D|��D}~�D}��D~~�D~��D~�D��D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D¿\D��\D�?\D�\Dÿ\D��\D�?\D�\DĿ\D��\D�?\D�\Dſ\D��\D�?\D�\Dƿ\D��\D�?\D�\Dǿ\D��\D�?\D�\Dȿ\D��\D�?\D�\Dɿ\D��\D�?\D�\Dʿ\D��\D�?\D�\D˿\D��\D�?\D�\D̿\D��\D�?\D�\DͿ\D��\D�?\D�\Dο\D��\D�?\D�\DϿ\D��\D�?\D�\Dп\D��\D�?\D�\Dѿ\D��\D�?\D�\Dҿ\D��\D�?\D�\Dӿ\D��\D�?\D�\DԿ\D��\D�?\D�\Dտ\D��\D�?\D�\Dֿ\D��\D�?\D�\D׿\D��)D�?\D�\Dؿ\D��\D�?\D�\Dٿ\D��\D�?\D�\Dڿ\D��\D�?\D�\Dۿ\D��\D�?\D�\Dܿ\D��\D�?\D�\Dݿ\D��\D�?\D�\D޿\D��\D�?\D�\D߿\D��\D�?\D�|)D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D�\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D��\D��\D�?\D�\D���D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bNA�`BA�bNA�ffA�jA�hsA�ffA�dZA�ffA�jA�hsA�ffA�bNA�bNA�dZA�`BA�dZA�dZA�dZA�bNA�ffA�ffA�n�A�jA�p�A�t�A�v�A�t�A�r�A�r�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�x�A�x�A�x�A�x�A�v�A�r�A�hsA�bNA�(�A��A��A�n�A�dZA��A�l�A�ĜA�~�A�"�A��A�$�A���A�+A�A��DA�VA�O�A�ĜA�-A�bNA���A�Q�A�p�A�7LA���A�x�A���A��hA��A��A�"�A�O�A���A���A�hsA�5?A���A�33A�oA��A��A�A�A�ĜA�%A��A��#A�-A���A��\A�5?A��!A�1'A���A�`BA�+A�&�A�
=A�~�A�n�A��A�^5A�bA�G�A�"�A��9A��
A�VA���A�A���A�K�A�A��A��AzM�Aw�
AvffAul�As�Ap  An�DAl�HAk\)Ai��Ah$�AgVAf�Af(�AeXAd��Ad5?Ab��AaA_�A_33A^=qA]��A[��AX�DAV�yAUx�AUVATr�AS7LAQ7LAO�AOƨAOO�AN�RAMXAK�mAKVAIAIK�AI�AH��AH��AHr�AG�hAEAD9XAC��AC��AB�/ABjAAƨA@��A?�TA?\)A>��A>VA=��A<��A;�#A;x�A;
=A:��A9�A8Q�A65?A4��A333A2VA0�9A/l�A-�A,��A,1'A*��A)�#A(��A&�yA%��A%�A$�HA#%A"��A" �A!��A!��A!l�A!A �A 5?A�`AM�A-A��AƨAXA�+A9XAp�A��A`BA��A$�AdZA��A1'A  A�A~�A�TAVA  A�!A?}A	�A�HAZA�
A�7A?}AoA^5A�FAVAƨA ��A   @�{@��@�=q@���@�@�5?@��;@�{@�O�@���@�Z@�t�@�$�@�O�@���@��@�\@�G�@�@�33@���@�ff@�M�@���@�`B@��`@�A�@ޏ\@��@ܼj@ܛ�@�|�@ٲ-@ؼj@�bN@���@�;d@��`@ҸR@Ѻ^@�%@�(�@���@��@�~�@͉7@�A�@˥�@���@�@�I�@�J@ũ�@�x�@�O�@��/@�z�@�Z@�Q�@� �@��;@öF@�t�@°!@�`B@�Q�@��P@�K�@���@��\@�=q@��^@�%@�(�@��m@��F@�"�@�5?@�p�@�r�@��;@��P@���@�(�@�@�{@�A�@��F@�t�@��@��@���@�Ĝ@���@�r�@�A�@�ƨ@��@�
=@���@���@��@���@��@���@�I�@��@��@�\)@��!@���@�O�@�%@��@�l�@�@���@�M�@�@��#@���@��7@���@�t�@��-@�hs@�G�@�7L@�V@��`@���@��j@��9@��m@�C�@���@�~�@�5?@�-@���@�
=@���@�\)@��y@��!@�5?@�@�`B@���@�z�@�9X@��@���@�|�@�;d@��!@�^5@�E�@�=q@���@��@���@��@��w@�K�@��\@�{@���@�hs@���@���@�Q�@�1@�o@��@�@���@�G�@��/@��@��@���@�l�@�C�@���@�v�@�@�`B@��@�I�@���@��@���@���@�n�@�V@�=q@�-@�-@�$�@�@���@���@�@��^@��@��@���@��D@���@�C�@�"�@�o@���@��@��!@���@��\@�ff@��@�x�@�`B@�7L@��@��@��@���@�Ĝ@���@��D@�;@~�+@}��@}��@}�@}O�@|Z@{C�@z�@z��@z��@z~�@y�@y��@y��@xb@w\)@w
=@v��@u��@u�h@u?}@u�@uV@t��@t�j@t�@t�D@tj@tZ@t1@s�@so@r��@r-@q�@qX@q7L@p�u@n��@n�@n��@m�@mV@k�m@k�F@kC�@ko@j�H@j�!@jM�@i��@ihs@i7L@i&�@i�@h��@h�`@h��@h�`@hĜ@h�@hb@g��@g\)@g;d@g+@g�@f��@f5?@e�@d�j@c�
@c33@b��@b~�@b=q@a��@a�#@a��@a�7@aG�@a�@`��@`��@`��@`bN@_�@_�w@_|�@^��@^@]��@]@]��@]p�@]�@\(�@[ƨ@[S�@[33@[o@Z�H@Z��@Z�H@Z��@ZM�@ZJ@Y��@Y�@Y�^@Y��@Y&�@Y%@X�9@X�u@X�@Xr�@XQ�@XA�@XA�@Xb@Wl�@V��@VE�@Up�@U�@UV@T�/@T1@S��@St�@S33@R~�@R-@R�@Q��@Q�7@Qhs@QG�@Q&�@P��@PA�@OK�@N��@O+@N��@Mp�@MV@MV@L�@L�D@Lz�@LZ@L(�@Kƨ@KS�@Ko@J�H@J~�@J=q@JJ@I��@Ix�@H1'@G�;@G�@G|�@GK�@F��@F$�@F$�@E��@E?}@D��@D�@D�@DI�@D(�@D(�@C�@CS�@C33@B�@B��@A�#@@��@@1'@?��@?l�@>�y@>v�@=��@=O�@=�@<�D@;�F@;33@:~�@:-@:J@:�@9��@9�@9�@9�#@9�@9�@9�@9�@9�@9�#@9�^@9��@9�^@9�#@8�u@7��@7�@7
=@7
=@7
=@7
=@7
=@7
=@7
=@6��@6ff@6V@5@5��@5p�@4�@4z�@4I�@49X@4�@41@3��@3��@3C�@333@2�H@2n�@1��@1�^@1��@1&�@0�@0  @/|�@/\)@/K�@/+@.��@.�y@.ȴ@.$�@.{@.@-@-?}@-?}@-�@,��@,�j@,I�@,�@+ƨ@+��@+dZ@+"�@+@*�H@*��@*n�@*=q@)��@)��@)G�@)7L@)%@(��@(bN@(A�@(b@(b@'�@'�@'\)@&�y@&�@&�@&��@&ff@&$�@&@%�T@%�-@%/@$�@$��@$��@$9X@#��@#t�@#o@#@"�!@"n�@"M�@"J@!hs@ ��@ �u@ r�@ bN@ bN@ 1'@�w@�P@l�@+@�R@V@�T@�h@�@p�@?}@��@�/@��@�@9X@�@�
@t�@C�@�H@��@n�@M�@J@�^@��@��@�7@hs@&�@��@ �@  @l�@
=@ȴ@��@��@ff@��@O�@�@z�@9X@�@ƨ@��@C�@o@�H@��@��@~�@~�@n�@M�@=q@-@��@��@��@�^@�7@x�@hs@7L@�@�9@r�@r�@r�@bN@bN@Q�@ �@ �@b@b@  @�@��@�w@�@��@��@�P@l�@;d@�@��@ȴ@�R@�R@��@5?@{@�@�@��@@�-@��@�h@�@`B@/@�@Z@�@�
@dZ@C�@33@o@
�H@
��@
��@
^5@
�@
J@	��@	��@	��@	�7@	x�@	hs@	X@	X@	G�@	G�@	�@Ĝ@r�@Q�@Q�@Q�@Q�@Q�@1'@�w@|�@K�@+@�y@��@v�@v�@ff@V@5?@�@�-@�h@p�@p�@p�@O�@�@V@��@�@�/@�/@��@�@��@�D@Z@I�@9X@9X@9X@�m@��@�@C�@"�@�H@��@��@�!@^5@-@�@��@��@�^@��@�7@�7@hs@7L@7L@7L@7L@7L@&�@ ��@ �@ bN@ 1'@ 1'?��;?��w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�bNA�`BA�bNA�ffA�jA�hsA�ffA�dZA�ffA�jA�hsA�ffA�bNA�bNA�dZA�`BA�dZA�dZA�dZA�bNA�ffA�ffA�n�A�jA�p�A�t�A�v�A�t�A�r�A�r�A�t�A�t�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�x�A�x�A�x�A�x�A�v�A�r�A�hsA�bNA�(�A��A��A�n�A�dZA��A�l�A�ĜA�~�A�"�A��A�$�A���A�+A�A��DA�VA�O�A�ĜA�-A�bNA���A�Q�A�p�A�7LA���A�x�A���A��hA��A��A�"�A�O�A���A���A�hsA�5?A���A�33A�oA��A��A�A�A�ĜA�%A��A��#A�-A���A��\A�5?A��!A�1'A���A�`BA�+A�&�A�
=A�~�A�n�A��A�^5A�bA�G�A�"�A��9A��
A�VA���A�A���A�K�G�O�G�O�A��AzM�Aw�
AvffAul�As�Ap  An�DAl�HAk\)Ai��Ah$�AgVAf�Af(�AeXAd��Ad5?Ab��AaA_�A_33A^=qA]��A[��AX�DAV�yAUx�AUVATr�AS7LAQ7LAO�AOƨAOO�AN�RAMXAK�mAKVAIAIK�AI�AH��AH��AHr�AG�hAEAD9XAC��AC��AB�/ABjAAƨA@��A?�TA?\)A>��A>VA=��A<��A;�#A;x�A;
=A:��A9�A8Q�A65?A4��A333A2VA0�9A/l�A-�A,��A,1'A*��A)�#A(��A&�yA%��A%�A$�HA#%A"��A" �A!��A!��A!l�A!A �A 5?A�`AM�A-A��AƨAXA�+A9XAp�A��A`BA��A$�AdZA��A1'A  A�A~�A�TAVA  A�!A?}A	�A�HAZA�
A�7A?}AoA^5A�FAVAƨA ��A   @�{@��@�=q@���@�@�5?@��;@�{@�O�@���@�Z@�t�@�$�@�O�@���@��@�\@�G�@�@�33@���@�ff@�M�@���@�`B@��`@�A�@ޏ\@��@ܼj@ܛ�@�|�@ٲ-@ؼj@�bN@���@�;d@��`@ҸR@Ѻ^@�%@�(�@���@��@�~�@͉7@�A�@˥�@���@�@�I�@�J@ũ�@�x�@�O�@��/@�z�@�Z@�Q�@� �@��;@öF@�t�@°!@�`B@�Q�@��P@�K�@���@��\@�=q@��^@�%@�(�@��m@��F@�"�@�5?@�p�@�r�@��;@��P@���@�(�@�@�{@�A�@��F@�t�@��@��@���@�Ĝ@���@�r�@�A�@�ƨ@��@�
=@���@���@��@���@��@���@�I�@��@��@�\)@��!@���@�O�@�%@��@�l�@�@���@�M�@�@��#@���@��7@���@�t�@��-@�hs@�G�@�7L@�V@��`@���@��j@��9@��m@�C�@���@�~�@�5?@�-@���@�
=@���@�\)@��y@��!@�5?@�@�`B@���@�z�@�9X@��@���@�|�@�;d@��!@�^5@�E�@�=q@���@��@���@��@��w@�K�@��\@�{@���@�hs@���@���@�Q�@�1@�o@��@�@���@�G�@��/@��@��@���@�l�@�C�@���@�v�@�@�`B@��@�I�@���@��@���@���@�n�@�V@�=q@�-@�-@�$�@�@���@���@�@��^@��@��@���@��D@���@�C�@�"�@�o@���@��@��!@���@��\@�ff@��@�x�@�`B@�7L@��@��@��@���@�Ĝ@���@��D@�;@~�+@}��@}��@}�@}O�@|Z@{C�@z�@z��@z��@z~�@y�@y��@y��@xb@w\)@w
=@v��@u��@u�h@u?}@u�@uV@t��@t�j@t�@t�D@tj@tZ@t1@s�@so@r��@r-@q�@qX@q7L@p�u@n��@n�@n��@m�@mV@k�m@k�F@kC�@ko@j�H@j�!@jM�@i��@ihs@i7L@i&�@i�@h��@h�`@h��@h�`@hĜ@h�@hb@g��@g\)@g;d@g+@g�@f��@f5?@e�@d�j@c�
@c33@b��@b~�@b=q@a��@a�#@a��@a�7@aG�@a�@`��@`��@`��@`bN@_�@_�w@_|�@^��@^@]��@]@]��@]p�@]�@\(�@[ƨ@[S�@[33@[o@Z�H@Z��@Z�H@Z��@ZM�@ZJ@Y��@Y�@Y�^@Y��@Y&�@Y%@X�9@X�u@X�@Xr�@XQ�@XA�@XA�@Xb@Wl�@V��@VE�@Up�@U�@UV@T�/@T1@S��@St�@S33@R~�@R-@R�@Q��@Q�7@Qhs@QG�@Q&�@P��@PA�@OK�@N��@O+@N��@Mp�@MV@MV@L�@L�D@Lz�@LZ@L(�@Kƨ@KS�@Ko@J�H@J~�@J=q@JJ@I��@Ix�@H1'@G�;@G�@G|�@GK�@F��@F$�@F$�@E��@E?}@D��@D�@D�@DI�@D(�@D(�@C�@CS�@C33@B�@B��@A�#@@��@@1'@?��@?l�@>�y@>v�@=��@=O�@=�@<�D@;�F@;33@:~�@:-@:J@:�@9��@9�@9�@9�#@9�@9�@9�@9�@9�@9�#@9�^@9��@9�^@9�#@8�u@7��@7�@7
=@7
=@7
=@7
=@7
=@7
=@7
=@6��@6ff@6V@5@5��@5p�@4�@4z�@4I�@49X@4�@41@3��@3��@3C�@333@2�H@2n�@1��@1�^@1��@1&�@0�@0  @/|�@/\)@/K�@/+@.��@.�y@.ȴ@.$�@.{@.@-@-?}@-?}@-�@,��@,�j@,I�@,�@+ƨ@+��@+dZ@+"�@+@*�H@*��@*n�@*=q@)��@)��@)G�@)7L@)%@(��@(bN@(A�@(b@(b@'�@'�@'\)@&�y@&�@&�@&��@&ff@&$�@&@%�T@%�-@%/@$�@$��@$��@$9X@#��@#t�@#o@#@"�!@"n�@"M�@"J@!hs@ ��@ �u@ r�@ bN@ bN@ 1'@�w@�P@l�@+@�R@V@�T@�h@�@p�@?}@��@�/@��@�@9X@�@�
@t�@C�@�H@��@n�@M�@J@�^@��@��@�7@hs@&�@��@ �@  @l�@
=@ȴ@��@��@ff@��@O�@�@z�@9X@�@ƨ@��@C�@o@�H@��@��@~�@~�@n�@M�@=q@-@��@��@��@�^@�7@x�@hs@7L@�@�9@r�@r�@r�@bN@bN@Q�@ �@ �@b@b@  @�@��@�w@�@��@��@�P@l�@;d@�@��@ȴ@�R@�R@��@5?@{@�@�@��@@�-@��@�h@�@`B@/@�@Z@�@�
@dZ@C�@33@o@
�H@
��@
��@
^5@
�@
J@	��@	��@	��@	�7@	x�@	hs@	X@	X@	G�@	G�@	�@Ĝ@r�@Q�@Q�@Q�@Q�@Q�@1'@�w@|�@K�@+@�y@��@v�@v�@ff@V@5?@�@�-@�h@p�@p�@p�@O�@�@V@��@�@�/@�/@��@�@��@�D@Z@I�@9X@9X@9X@�m@��@�@C�@"�@�H@��@��@�!@^5@-@�@��@��@�^@��@�7@�7@hs@7L@7L@7L@7L@7L@&�@ ��@ �@ bN@ 1'@ 1'?��;?��w1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bt�Bt�Bu�Bt�Bs�Bs�Bt�Bt�Bu�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bt�Bt�Bu�Bu�Bv�Bt�Bu�Bu�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bu�Bt�Bs�Bo�BhsBN�BR�BjBcTBjBbNBXBw�B�By�B`BBE�BiyBe`BT�B.BJB��B1'B-B�B�B%B�B�BɺB��B�5B�HB�#B�B��B�LB�^B�B��B�wB�'B�9B�LB�B�{B~�B�+Bs�B_;BS�BVB\)BW
BN�B>wB.BuB
��B�B!�B�BB
�mB
�TB
��B
��B
�-B
ŢB
�RB
��B
��B
�uB
�VB
�VB
�%B
y�B
bNB
)�B	�ZB
B
�B
+B	�B	��B	�B	��B	��B	��B	�qB	��B	ɺB	ĜB	�RB	�RB	�B	��B	�7B	�\B	�PB	�B	v�B	S�B	>wB	K�B	J�B	YB	M�B	;dB	)�B	/B	@�B	6FB	)�B	�B	\B	�B	hB	�B	"�B	 �B	�B	bB��B�B�B	B	%B��B��B�B�yB�ZB�B�B�TB�#B�B��B��B��BŢB�}B��B�uB�hB��B��B�B�%B~�B~�B�Bp�Bo�BdZBe`BbNBw�BffBR�Bp�Bs�Bq�Bz�Bw�Bn�Bq�BdZBVBbNBo�Bk�BffB`BBT�BZBM�BI�B:^BD�BD�B8RB)�BJB%B-B-B)�B�BoBJB��B��B��B'�B&�B,B)�B&�B�B�B{BJBDB�BhB\BoBhB�B�B\B�B&�B,B(�B$�B�B�B�B��BhB�B#�B�B(�B-B.B+B$�B$�B�B�B�B'�B&�B�BuB �B'�B"�B�B
=B\B#�B(�B)�B.B+B(�B%�B#�B,B(�B%�B"�B �B@�BF�BF�BD�BE�BI�BJ�BG�BD�BD�B@�B8RB7LB;dBC�BK�BI�BK�BJ�BG�BE�BF�BO�BN�BG�BF�BF�BI�BK�BM�B>wBB�BK�BQ�BM�B^5Be`BcTB_;BdZBp�Br�Br�Bs�Br�Bu�Bs�Bp�B{�B�B|�Bz�B}�B�B�B�B�B~�Bz�B�+B�1B�%B�B�hB�oB��B��B�B�!B�B��B��B��B�B�'B�-B�-B�3B�9B�3B�'B�B�B�-B�qB�jB��B��B��B��BȴBɺB��B��B��B��B��B��B��B�B�
B�B�B�
B�#B�;B�5B�B�
B�5B�5B�ZB�fB�sB�B�B��B��B��B	  B	B��B	B	oB	�B	�B	�B	�B	�B	�B	&�B	'�B	%�B	)�B	)�B	,B	0!B	1'B	7LB	A�B	J�B	O�B	R�B	T�B	VB	W
B	XB	XB	XB	\)B	\)B	^5B	^5B	\)B	[#B	]/B	_;B	\)B	ffB	k�B	m�B	l�B	m�B	m�B	n�B	n�B	m�B	l�B	p�B	w�B	x�B	z�B	{�B	z�B	x�B	w�B	y�B	{�B	y�B	y�B	�B	�%B	�B	�B	~�B	�B	�+B	�DB	�DB	�DB	�7B	�JB	�JB	�1B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�3B	�3B	�9B	�?B	�?B	�9B	�?B	�XB	�dB	�jB	�jB	�jB	�qB	�wB	�qB	�jB	�jB	�jB	�qB	��B	ĜB	ŢB	ŢB	ŢB	ŢB	ƨB	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�B	�#B	�#B	�)B	�)B	�#B	�/B	�NB	�TB	�fB	�`B	�`B	�NB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B
  B	��B	��B
B
B
B
B
B
B
B
B
  B	��B
B
B
B	��B
B
	7B
1B

=B
JB
DB

=B

=B

=B
JB
JB
DB
PB
PB
PB

=B
+B
\B
hB
bB
bB
VB
bB
uB
oB
bB
{B
�B
{B
{B
�B
�B
{B
�B
�B
�B
{B
hB
oB
�B
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
#�B
&�B
&�B
&�B
&�B
&�B
&�B
'�B
'�B
&�B
&�B
&�B
&�B
%�B
&�B
$�B
#�B
�B
 �B
%�B
+B
+B
+B
+B
+B
+B
)�B
'�B
'�B
(�B
&�B
)�B
(�B
'�B
(�B
,B
-B
-B
-B
,B
+B
,B
-B
,B
+B
,B
.B
/B
,B
-B
.B
0!B
49B
49B
49B
33B
49B
33B
1'B
5?B
6FB
49B
49B
7LB
7LB
6FB
5?B
5?B
7LB
7LB
8RB
9XB
9XB
:^B
:^B
:^B
9XB
:^B
:^B
:^B
;dB
<jB
<jB
;dB
=qB
>wB
>wB
?}B
>wB
=qB
=qB
>wB
@�B
@�B
@�B
?}B
?}B
A�B
@�B
@�B
?}B
A�B
B�B
B�B
@�B
@�B
D�B
C�B
E�B
D�B
D�B
E�B
D�B
B�B
E�B
F�B
I�B
I�B
I�B
H�B
G�B
I�B
I�B
H�B
G�B
H�B
I�B
K�B
M�B
L�B
L�B
L�B
M�B
N�B
M�B
K�B
M�B
M�B
L�B
N�B
N�B
O�B
P�B
Q�B
P�B
P�B
Q�B
S�B
R�B
Q�B
P�B
O�B
O�B
S�B
P�B
S�B
T�B
VB
VB
T�B
R�B
S�B
T�B
XB
YB
YB
YB
YB
YB
ZB
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
\)B
\)B
^5B
^5B
]/B
^5B
^5B
]/B
]/B
]/B
^5B
`BB
`BB
`BB
`BB
`BB
_;B
`BB
aHB
aHB
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
aHB
aHB
bNB
bNB
aHB
`BB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
aHB
`BB
aHB
cTB
cTB
cTB
e`B
gmB
ffB
ffB
ffB
ffB
ffB
ffB
hsB
gmB
hsB
iyB
jB
jB
jB
jB
jB
jB
iyB
iyB
hsB
iyB
k�B
l�B
l�B
l�B
k�B
jB
iyB
jB
l�B
l�B
l�B
l�B
n�B
o�B
o�B
n�B
n�B
m�B
n�B
o�B
o�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
p�B
q�B
r�B
r�B
q�B
p�B
p�B
r�B
q�B
r�B
r�B
s�B
t�B
s�B
r�B
s�B
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
u�B
t�B
t�B
v�B
v�B
w�B
v�B
w�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bt�Bt�Bu�Bt�Bs�Bs�Bt�Bt�Bu�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bt�Bt�Bu�Bu�Bv�Bt�Bu�Bu�Bt�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bv�Bu�Bt�Bs�Bo�BiDBQ�BU�Bl�Bf�Bl�BeFB[�By	B��B{0Bc BIlBj�Bf�BVmB1�BB��B1�B.cBkB�B�B�B�/B�B�.B�;B�NB�xB��B�.B�rB�B��B�B�B��B�B��B��B��B� B��Bu�Ba�BV�BWsB\�BW�BO�B?�B/�BmB
�PB�B!�BEB�B
��B
��B
�B
�tB
��B
��B
��B
��B
�B
�2B
��B
�BB
�B
z�G�O�G�O�B	�KB
�B
�B
�B	�B	�HB	��B	� B	̘B	�aB	�}B	��B	�=B	�SB	�XB	�	B	�B	�xB	�DB	��B	�<B	�[B	xB	V�B	BAB	M�B	LdB	Y�B	N�B	=<B	,�B	0oB	@�B	7B	+B	kB	B	�B	B	/B	# B	!B	B	B�}B�B�MB	�B	tB��B��B�B�B�B�CB�B�@B�CB�	B�(B�{B�}BƎB��B�TB�9B��B�?B�B�zB��B��B�iB�Br�Bp�Bf2BgmBc�BxBg�BT�BqABtTBraB{Bx8BoOBq�Be�BW�BcBo�Bk�Bf�B`�BV9BZ�BOBKB<PBE�BEmB9�B+�B�B	B-�B.B*�BBB"B;B��B��B(XB'�B,�B*B'mB�B�B�BB�B�B�B�B�B�BpBjBB�B'�B,qB)yB%�B�B �B�B��B�B�B$ZB�B)_B-]B.cB+QB%zB%`B vB�B�B($B'RB�B�B!bB(>B#TBqBB�B$�B)�B*�B.cB+�B)�B&�B$�B,�B)�B&�B$B"�B@�BF�BF�BEBFBI�BJ�BG�BD�BD�B@�B9>B8RB<6BDBLBJ#BLBKBH1BF?BG_BPBO(BHfBG_BG_BJ�BLJBNVB@ BC�BL�BR�BOBB^�Be�Bc�B`'BeBp�Br�Br�Bs�BsBvBtTBqvB|B�B}<B{dB~]B�[B�[B�uB�[B�B{�B�zB��B��B�'B��B��B��B�+B��B�oB�}B��B��B��B�IB�AB�GB�aB�hB�TB�MB�[B��B��B��B�qB��B��BʌB��BΊB�B�#B�B�)B�0B�0B�0B�.B�2B�SB�YB�QB�eB�sB�WB�VB�jBچBרBޞB޸B�B��B��B�B�B�$B�>B�HB	 iB	[B��B	�B	�B	�B	�B	�B	�B	B	 'B	'B	($B	&LB	*eB	*eB	,�B	0�B	1�B	7�B	A�B	J�B	PB	S&B	U2B	VB	W$B	XB	X+B	X_B	\CB	\CB	^OB	^OB	\]B	[qB	]~B	_�B	\�B	f�B	k�B	m�B	l�B	m�B	m�B	n�B	n�B	m�B	l�B	p�B	w�B	y	B	z�B	|B	z�B	y	B	xB	zB	|B	z^B	z^B	�GB	�?B	�MB	�GB	}B	�oB	�EB	�DB	�xB	�^B	��B	�dB	�dB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	� B	��B	� B	�5B	�B	�0B	�DB	�_B	�qB	�MB	�hB	�TB	�ZB	�ZB	�nB	�tB	�rB	�B	��B	��B	��B	��B	��B	�qB	��B	��B	��B	��B	��B	ĶB	��B	żB	��B	��B	�B	�B	�6B	�B	�B	�B	�$B	�+B	�KB	�KB	�1B	�KB	�7B	�=B	�CB	�QB	�WB	�WB	�CB	�]B	�qB	�dB	�B	�nB	�B	�zB	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	�B	�B
 B	�B	�<B
;B
B
'B
AB
MB
-B
AB
;B
 OB	�HB
-B
9B
UB	�}B
9B
	7B
fB

rB
JB
^B

XB

rB

rB
dB
dB
xB
jB
jB
jB

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
$B
&�B
&�B
'B
&�B
'B
&�B
'�B
'�B
&�B
&�B
&�B
&�B
%�B
'B
$�B
#�B
/B
!-B
&B
+B
+B
+B
+B
+B
+B
)�B
(>B
(
B
)B
'B
)�B
)B
($B
)*B
,"B
-)B
-)B
-)B
,"B
+QB
,"B
-)B
,=B
+6B
,=B
./B
/OB
,WB
-CB
.IB
0UB
49B
4TB
4nB
3MB
4nB
3MB
1vB
5?B
6`B
4TB
4nB
7LB
7�B
6`B
5ZB
5tB
7fB
7fB
8�B
9rB
9rB
:�B
:xB
:xB
9rB
:�B
:�B
:�B
;B
<�B
<�B
;B
=�B
>�B
>�B
?}B
>�B
=�B
=�B
>�B
@�B
@�B
@�B
?�B
?�B
A�B
@�B
@�B
?�B
A�B
B�B
B�B
@�B
@�B
D�B
C�B
E�B
D�B
D�B
E�B
D�B
B�B
E�B
F�B
I�B
I�B
I�B
H�B
G�B
I�B
I�B
H�B
G�B
H�B
I�B
K�B
M�B
L�B
L�B
L�B
M�B
N�B
M�B
LB
M�B
M�B
MB
N�B
OB
PB
Q B
RB
Q B
Q B
RB
S�B
SB
RB
Q B
PB
P.B
S�B
QB
TB
UB
VB
V9B
UB
S@B
TFB
U2B
X+B
Y1B
Y1B
Y1B
Y1B
YB
ZQB
[=B
\)B
\)B
\]B
]/B
]IB
]IB
]/B
]/B
\]B
\CB
^5B
^OB
]dB
^5B
^5B
]dB
]dB
]IB
^OB
`BB
`BB
`BB
`BB
`BB
_VB
`'B
aHB
aHB
`BB
`BB
`\B
aHB
abB
aHB
aHB
abB
`\B
`vB
`vB
aHB
abB
bNB
bNB
abB
`vB
bhB
bhB
cTB
cnB
cTB
c:B
cTB
cTB
cTB
bhB
a|B
`vB
abB
cnB
cnB
c�B
ezB
gmB
f�B
f�B
f�B
f�B
ffB
f�B
hsB
g�B
h�B
iyB
j�B
jB
jB
jB
jB
jB
i�B
i�B
h�B
i�B
k�B
l�B
l�B
l�B
k�B
j�B
i�B
j�B
l�B
l�B
l�B
l�B
n�B
o�B
o�B
n�B
n�B
m�B
n�B
o�B
o�B
p�B
p�B
o�B
o�B
p�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
q�B
p�B
q�B
r�B
r�B
q�B
p�B
p�B
r�B
q�B
r�B
r�B
s�B
t�B
s�B
r�B
s�B
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
u�B
t�B
t�B
v�B
v�B
w�B
v�B
w�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.02(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901310036152019013100361520190131003615201901310200172019013102001720190131020017201902010024522019020100245220190201002452  JA  ARFMdecpA19c                                                                20190127093629  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190127003633  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190127003636  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190127003637  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190127003638  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190127003638  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190127003638  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20190127003638  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20190127003638  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190127003638  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20190127003639  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190127003639                      G�O�G�O�G�O�                JA  ARUP                                                                        20190127005749                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190127153318  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20190127153318  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20190127153318  CV  LATITUDE        G�O�G�O�A���                JM  ARGQJMQC2.0                                                                 20190127153318  CV  LONGITUDE       G�O�G�O��"�                JM  ARCAJMQC2.0                                                                 20190130153615  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190130153615  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20190130170017  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190131152452  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                