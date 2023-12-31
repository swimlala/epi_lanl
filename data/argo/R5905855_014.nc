CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:13:09Z creation;2022-06-04T19:13:09Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604191309  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�Ȑ��l1   @�Ȑ��s�@.��t�j�d�E���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  BB  BF��BP  BW33B_��Bh  Bp  Bx  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�ffB���B�  B�  B�  C   C  C  C�C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8L�C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj�Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DI��DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�C3Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @p�@s�
@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qBA=qBF
>BO=qBVp�B^�Bg=qBo=qBw=qB=qB���B���B���B�8RB�k�B���B���B���B���B���B���B���B���B���B�8RB�k�BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B�k�B�B�k�B�B���B���B���C�\C�\C��C��C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5��C8)C9�\C;��C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg��Ci��Ck��Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��{C��C��C��C��C��C��{C��{C��{C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Dz=D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI�qDJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�=D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�=D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�pR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aݱ'Aݮ�Aݣ:AݬAݝ�AݔFA�ZA�E9A��`Aܨ�Aܞ�A܏�A�j�A۲-A��A���A��A��dA���AڿHAڞ�AړAځ�A�d�A�GA���A�@OA��Aԇ+A��A�.IA���A�:A��A��`AΏ�A�,�A��DA�� AɌA�4�AȻ�A��AǍPA�FA�)*A�ݘAÞ�A��CA���A�[�A�hA�qA�ΥA�+6A�qAA���A�A�]/A�gA���A�aHA�7A���A��@A��;A�"�A�GzA�<A�-wA�h
A�$A���A���A��aA��,A�)�A��HA�ȀA��LA��	A��A�MA��A�8RA�2�A�`�A�yrA��*A���A��aA��A�S[A{� Au|�Aq�[Ao�Ala|Ah�bAf��Ab&�A^�A[]�AYh
AW7AT#�AQ9�AO�RAN�hAK1�AI��AH�mAF�AD�)AC[WA@qvA>W?A=�A;��A:W?A8�A7�oA6�+A5P�A3!-A0��A-z�A,:*A)�HA( iA&�2A%��A$/�A"M�A! \A \�A��A�AA�AG�A�-AĜA��A4A�AC�A:�A \A��A�PAW?AH�A-A=�AV�A%A�zAJ�A�9A��A�A�A��A�A�YA��Ao A�KA�A��A|�AffA�}A�A�DAU2A��A��A/Aa�A
�uA	��A	�MA
�}A
Q�A
MjA
h�A
�NA
-wA	��A�AA~�A,=A�4A	A�A8�AAGA��A�<A�}A��A�DA�A�FAQAu�Am�A @�@��@�K^@��@�&@��@��@�q@���@�S@�F@��)@�@�1�@��A@��a@�n/@��@��,@��'@�@��6@���@���@�b�@�ߤ@��]@�Ov@�҉@�E9@�k@�҉@�c�@��@�x�@� @�/�@��@�Y@��@�^5@���@�
=@䲖@�@@��@�B[@�0@�@��T@���@ޛ�@�v�@�ں@�S@��@٬q@�IR@��[@�_@״�@�4@��M@�Ɇ@�c @��@��@�x@��,@�Ĝ@ԧ@Ԗ�@�K^@Ӿw@�t�@�S&@�;@�Ĝ@Ғ�@�7�@�ݘ@�-w@���@�b�@��v@΃@�1'@�� @�^�@���@��/@�9X@˼@�q@�M�@��]@�N<@�J�@ǨX@��@�1'@ţn@��@�h
@�e@��o@Ü�@ @�!@��@�b�@�a@�J�@�ں@�Z@���@�W?@��@��M@���@�L0@���@��@�hs@��B@�PH@�A @�� @��@�e,@�0�@�Ov@���@�|@�Z�@�8@��s@��@��@���@���@��o@�Ov@��)@��S@��9@�w�@�s�@�@��'@��@��T@��$@�F�@�&@��@��K@���@���@�_�@��P@�&�@��"@���@��>@���@�c@�x@�W?@�"�@��4@��r@�hs@�&�@��'@��.@� �@�خ@��-@�P�@��c@��R@���@�($@� �@�A�@��]@��_@�V@�	�@�|@��@��@���@��$@���@�J�@��@���@�@O@��@��j@��@��@���@���@��-@�|�@�Mj@�@��H@���@�!@��@��@��0@�L�@�@�ߤ@��s@��,@���@���@��@��@�X@�P�@��@��@���@�|�@�e�@�g8@�D�@�?�@�Xy@�\�@�C-@�x@��4@���@��@�a@�.I@��P@�4@���@�E9@��H@�l�@���@�e�@�%F@�e�@�3�@�!�@� �@��@��:@��@�r�@�Q@�1�@�J@��@�_@���@��-@��@�}�@�^�@�+@���@�kQ@�7@��*@�<6@��@���@���@�y>@�_�@���@�@���@���@�qv@�5�@�U2@�@�@���@��@��@���@�H�@���@�1'@��>@���@�C@���@��_@���@�?@�O@���@��P@�l�@�O@�=@�4@�(@��@��p@��j@�u%@�?@��@���@���@��	@�W?@��@�%@���@���@�ѷ@���@�z@�i�@�&�@��@�X@��@��K@���@��j@���@�I�@� �@��Q@��0@�p�@�IR@��@���@�z@�($@�m@��@H�@~�R@~E�@~#:@}�n@|PH@{�+@{X�@z�H@z�F@zZ�@y�@x�P@x��@xPH@x1@w��@w\)@v��@v��@v\�@ue,@u@uV@u	l@t��@t@st�@r��@r��@r��@rB[@r�@q�#@q\�@p�@pQ�@pFt@p1'@p%�@o��@o�*@o{J@o>�@n�@n1�@mj@l�@l	�@k��@k$t@j��@jz@jQ@jH�@jB[@j($@i�>@h�9@g��@gخ@g��@gMj@g�@f�}@f �@eY�@d��@e<6@eG�@dĜ@c��@c@b�+@bR�@b0U@b�@a��@a<6@a/@a	l@`��@`q@`/�@_��@_K�@_,�@^�8@^ȴ@^��@^u%@^5?@]�3@]O�@\��@\?�@[�V@[9�@[(@Z��@ZE�@Y��@Y^�@Y+�@X�5@X�@X:�@W�F@WC�@V�6@VO@U�S@U�@T�e@T9X@S��@S�*@Sy�@S@R��@R=q@Q��@Q \@P�)@PS�@O�W@O��@On/@O�@N��@N�'@N�@Nc @NW�@N	@M��@MQ�@Ly>@L-�@LM@K�W@K��@K��@Ko@J�L@J��@J6�@I��@I�@Io @I5�@H�P@H�4@H�@G��@G|�@G.I@F�@F!�@E��@E8�@E�@D�$@D�@D4n@D�@DG@C�@C�@CE9@C$t@C�@B҉@B�\@B=q@B �@A�3@AS&@@�f@@�z@@q@@/�@?�@?��@?C@>i�@=��@=��@=IR@<�5@<֡@<�@<bN@<  @;~�@:��@:��@:n�@9�d@9u�@9Dg@8��@8g8@8 �@8G@7�@7qv@7C@6҉@6��@5�D@5�"@5*0@4�`@4�e@4�D@4e�@4PH@4>B@44n@4G@3��@3]�@2��@2�@2u%@2YK@2;�@1��@1Y�@1=�@1	l@0��@0Ĝ@0��@/��@/�@@/s@/33@.��@.1�@.�@-��@-|@-�@,�O@,�D@,l"@,H@+�;@+{J@+\)@+/�@*�s@*z@*J�@)��@)�@)�t@)u�@)B�@)*0@)�@(�p@(�j@(�.@(V�@(,=@'��@'��@'\)@'�@&�y@&�m@&�F@&{@%��@%+@$�@$�/@$��@$H@$�@$@#�@#��@#X�@#/�@"�B@"�<@"�R@"��@"Ta@"�@!��@!@!��@!��@!`B@!Vm@!-w@ �@ ��@ [�@ I�@ �@�
@�a@��@dZ@)_@S@ں@��@��@xl@J�@#:@��@�n@+@�@b@�w@��@��@e�@C@�@��@d�@�@ԕ@�-@��@��@c@0�@�@y>@e�@,=@�6@�0@��@�4@6z@�@�@{�@B[@$�@ �@�#@�X@��@|@a�@A @@@��@��@�z@��@�@h�@`�@PH@-�@�]@�;@˒@��@_p@6z@�@�s@��@�r@0U@�D@��@x�@e,@=�@%@�)@�@S�@-�@��@�m@�0@�@X�@@�'@n�@L0@-@�@��@��@m]@B�@�@��@��@�U@��@y>@D�@�@��@�K@�q@�{@]�@9�@�@
��@
�2@
�X@
�'@
��@
d�@
5?@
)�@
O@	��@	�>@	�X@	c�@	B�@	�@�|@��@�e@�@oi@D�@7@1@�@خ@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aݱ'Aݮ�Aݣ:AݬAݝ�AݔFA�ZA�E9A��`Aܨ�Aܞ�A܏�A�j�A۲-A��A���A��A��dA���AڿHAڞ�AړAځ�A�d�A�GA���A�@OA��Aԇ+A��A�.IA���A�:A��A��`AΏ�A�,�A��DA�� AɌA�4�AȻ�A��AǍPA�FA�)*A�ݘAÞ�A��CA���A�[�A�hA�qA�ΥA�+6A�qAA���A�A�]/A�gA���A�aHA�7A���A��@A��;A�"�A�GzA�<A�-wA�h
A�$A���A���A��aA��,A�)�A��HA�ȀA��LA��	A��A�MA��A�8RA�2�A�`�A�yrA��*A���A��aA��A�S[A{� Au|�Aq�[Ao�Ala|Ah�bAf��Ab&�A^�A[]�AYh
AW7AT#�AQ9�AO�RAN�hAK1�AI��AH�mAF�AD�)AC[WA@qvA>W?A=�A;��A:W?A8�A7�oA6�+A5P�A3!-A0��A-z�A,:*A)�HA( iA&�2A%��A$/�A"M�A! \A \�A��A�AA�AG�A�-AĜA��A4A�AC�A:�A \A��A�PAW?AH�A-A=�AV�A%A�zAJ�A�9A��A�A�A��A�A�YA��Ao A�KA�A��A|�AffA�}A�A�DAU2A��A��A/Aa�A
�uA	��A	�MA
�}A
Q�A
MjA
h�A
�NA
-wA	��A�AA~�A,=A�4A	A�A8�AAGA��A�<A�}A��A�DA�A�FAQAu�Am�A @�@��@�K^@��@�&@��@��@�q@���@�S@�F@��)@�@�1�@��A@��a@�n/@��@��,@��'@�@��6@���@���@�b�@�ߤ@��]@�Ov@�҉@�E9@�k@�҉@�c�@��@�x�@� @�/�@��@�Y@��@�^5@���@�
=@䲖@�@@��@�B[@�0@�@��T@���@ޛ�@�v�@�ں@�S@��@٬q@�IR@��[@�_@״�@�4@��M@�Ɇ@�c @��@��@�x@��,@�Ĝ@ԧ@Ԗ�@�K^@Ӿw@�t�@�S&@�;@�Ĝ@Ғ�@�7�@�ݘ@�-w@���@�b�@��v@΃@�1'@�� @�^�@���@��/@�9X@˼@�q@�M�@��]@�N<@�J�@ǨX@��@�1'@ţn@��@�h
@�e@��o@Ü�@ @�!@��@�b�@�a@�J�@�ں@�Z@���@�W?@��@��M@���@�L0@���@��@�hs@��B@�PH@�A @�� @��@�e,@�0�@�Ov@���@�|@�Z�@�8@��s@��@��@���@���@��o@�Ov@��)@��S@��9@�w�@�s�@�@��'@��@��T@��$@�F�@�&@��@��K@���@���@�_�@��P@�&�@��"@���@��>@���@�c@�x@�W?@�"�@��4@��r@�hs@�&�@��'@��.@� �@�خ@��-@�P�@��c@��R@���@�($@� �@�A�@��]@��_@�V@�	�@�|@��@��@���@��$@���@�J�@��@���@�@O@��@��j@��@��@���@���@��-@�|�@�Mj@�@��H@���@�!@��@��@��0@�L�@�@�ߤ@��s@��,@���@���@��@��@�X@�P�@��@��@���@�|�@�e�@�g8@�D�@�?�@�Xy@�\�@�C-@�x@��4@���@��@�a@�.I@��P@�4@���@�E9@��H@�l�@���@�e�@�%F@�e�@�3�@�!�@� �@��@��:@��@�r�@�Q@�1�@�J@��@�_@���@��-@��@�}�@�^�@�+@���@�kQ@�7@��*@�<6@��@���@���@�y>@�_�@���@�@���@���@�qv@�5�@�U2@�@�@���@��@��@���@�H�@���@�1'@��>@���@�C@���@��_@���@�?@�O@���@��P@�l�@�O@�=@�4@�(@��@��p@��j@�u%@�?@��@���@���@��	@�W?@��@�%@���@���@�ѷ@���@�z@�i�@�&�@��@�X@��@��K@���@��j@���@�I�@� �@��Q@��0@�p�@�IR@��@���@�z@�($@�m@��@H�@~�R@~E�@~#:@}�n@|PH@{�+@{X�@z�H@z�F@zZ�@y�@x�P@x��@xPH@x1@w��@w\)@v��@v��@v\�@ue,@u@uV@u	l@t��@t@st�@r��@r��@r��@rB[@r�@q�#@q\�@p�@pQ�@pFt@p1'@p%�@o��@o�*@o{J@o>�@n�@n1�@mj@l�@l	�@k��@k$t@j��@jz@jQ@jH�@jB[@j($@i�>@h�9@g��@gخ@g��@gMj@g�@f�}@f �@eY�@d��@e<6@eG�@dĜ@c��@c@b�+@bR�@b0U@b�@a��@a<6@a/@a	l@`��@`q@`/�@_��@_K�@_,�@^�8@^ȴ@^��@^u%@^5?@]�3@]O�@\��@\?�@[�V@[9�@[(@Z��@ZE�@Y��@Y^�@Y+�@X�5@X�@X:�@W�F@WC�@V�6@VO@U�S@U�@T�e@T9X@S��@S�*@Sy�@S@R��@R=q@Q��@Q \@P�)@PS�@O�W@O��@On/@O�@N��@N�'@N�@Nc @NW�@N	@M��@MQ�@Ly>@L-�@LM@K�W@K��@K��@Ko@J�L@J��@J6�@I��@I�@Io @I5�@H�P@H�4@H�@G��@G|�@G.I@F�@F!�@E��@E8�@E�@D�$@D�@D4n@D�@DG@C�@C�@CE9@C$t@C�@B҉@B�\@B=q@B �@A�3@AS&@@�f@@�z@@q@@/�@?�@?��@?C@>i�@=��@=��@=IR@<�5@<֡@<�@<bN@<  @;~�@:��@:��@:n�@9�d@9u�@9Dg@8��@8g8@8 �@8G@7�@7qv@7C@6҉@6��@5�D@5�"@5*0@4�`@4�e@4�D@4e�@4PH@4>B@44n@4G@3��@3]�@2��@2�@2u%@2YK@2;�@1��@1Y�@1=�@1	l@0��@0Ĝ@0��@/��@/�@@/s@/33@.��@.1�@.�@-��@-|@-�@,�O@,�D@,l"@,H@+�;@+{J@+\)@+/�@*�s@*z@*J�@)��@)�@)�t@)u�@)B�@)*0@)�@(�p@(�j@(�.@(V�@(,=@'��@'��@'\)@'�@&�y@&�m@&�F@&{@%��@%+@$�@$�/@$��@$H@$�@$@#�@#��@#X�@#/�@"�B@"�<@"�R@"��@"Ta@"�@!��@!@!��@!��@!`B@!Vm@!-w@ �@ ��@ [�@ I�@ �@�
@�a@��@dZ@)_@S@ں@��@��@xl@J�@#:@��@�n@+@�@b@�w@��@��@e�@C@�@��@d�@�@ԕ@�-@��@��@c@0�@�@y>@e�@,=@�6@�0@��@�4@6z@�@�@{�@B[@$�@ �@�#@�X@��@|@a�@A @@@��@��@�z@��@�@h�@`�@PH@-�@�]@�;@˒@��@_p@6z@�@�s@��@�r@0U@�D@��@x�@e,@=�@%@�)@�@S�@-�@��@�m@�0@�@X�@@�'@n�@L0@-@�@��@��@m]@B�@�@��@��@�U@��@y>@D�@�@��@�K@�q@�{@]�@9�@�@
��@
�2@
�X@
�'@
��@
d�@
5?@
)�@
O@	��@	�>@	�X@	c�@	B�@	�@�|@��@�e@�@oi@D�@7@1@�@خ@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	A�B	A�B	A�B	AoB	A�B	A�B	A B	@�B	A;B	@�B	@B	?HB	=�B	6`B	0�B	/OB	.�B	.�B	/B	.cB	,�B	+�B	*�B	+B	,�B	2B	>�B	^�B	l�B	kkB	pUB	y$B	�XB	��B	z�B	�kB	�(B	��B	��B	��B	�UB
9�B
`B
sB
z*B
v�B
{�B
��B
ˬB
��B
��B
�B
�'B�B�B�B"�BA�BN�BUMBb4BlBt�B��B��B�B�1B�Bz�Bc�B}qB��B�Bs�BJ	B{B
�(B
�"B�B�B%`B �B	�B
�oB
��B
�RB
�vB
�B
��B
�HB
��B
zxB
$ZB	��B	��B	��B	�>B	�bB	{JB	lWB	W�B	C-B	33B	*�B	�B	�B	�B	 �B�B�tB�B��B�qB��B�FB�8BބB�dB�8B��B��B�B�B�B�VB�B�aB�BخB�B��B�>B�_B��B�B�B��B�`B��B��B	 �B	�B	�B	mB	pB	#�B	$�B	%�B	(sB	1�B	7�B	9>B	=�B	E�B	LJB	P�B	V�B	gB	t�B	u?B	y$B	xRB	u�B	�AB	�B	��B	�B	�	B	�pB	��B	��B	��B	��B	��B	�B	�WB	��B	�B	�;B	�B	��B	�B	��B	��B	�yB	��B	��B	�9B	�ZB	��B	�0B	�lB	��B	��B	�[B	��B	��B	�fB	�B	�RB	�PB	�XB	�(B	��B	��B	�XB	�	B	��B	�3B	�!B	�/B	�IB	�;B	�/B	��B	��B	�"B	��B	��B	��B	�GB	�qB	�B	��B	�B	�B	�NB	��B	��B	�LB	�2B	�2B	�WB	��B	��B	��B	��B	��B	��B	�eB	��B	�MB	�0B	�"B	��B	�cB	�AB	͟B	�BB	�PB	�#B	�GB	��B	�EB	�B	�B	�3B	ªB	� B	�iB	��B	��B	�HB	��B	��B	�qB	��B	��B	��B	�cB	�UB	�'B	�uB	żB	�B	�"B	�}B	�hB	�NB	� B	ЗB	�oB	�B	�MB	�sB	��B	�QB	��B	�B	�WB	��B	�B	�jB	ބB	�jB	ބB	��B	�;B	�;B	�VB	�BB	�vB	��B	��B	��B	�|B	�4B	�B	�4B	�B	��B	� B	�B	�4B	��B	�B	�B	�B	�fB	��B	��B	�B	�B	��B	��B	��B	�B	�5B	�iB	�B	�OB	�5B	�5B	�5B	�B	�B	�IB	�/B	��B	�B	��B	�B	��B	��B	�|B	�|B	�aB	�aB	�aB	�GB	�-B	�aB	�B	�B	�hB	�B	�B	��B	��B	��B	��B	�9B	��B	�B	�B	�ZB	�tB	��B	�tB	��B	��B	��B	��B	�zB	�fB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�XB	�B	�DB	�^B	��B	�0B	�0B	�PB	��B	�<B	��B	��B	�B	�wB	�B	�}B	��B	�cB	��B	�}B
  B	�cB	��B
 4B
 �B
�B
�B
�B
�B
�B
3B
�B
�B
�B
SB
�B
�B
gB
�B
�B
�B
_B
�B
_B
�B
B
+B
�B
�B
�B
fB
�B
KB
1B
�B
�B

#B
)B
^B
DB

�B

�B
�B
JB
�B
^B
B
^B

�B

�B
^B
�B
�B
6B
�B
�B
�B
�B
(B
BB
�B
bB
NB
�B
�B
:B
�B
�B
�B
�B
�B
�B
�B
�B
�B
1B
QB
�B
B
�B
B
dB
�B
B
�B
�B
�B
�B
�B
�B
 �B
 BB
 \B
 \B
 BB
 'B
!-B
!|B
!B
 BB
�B
�B
 vB
 �B
 �B
 �B
 �B
!-B
!|B
!�B
"B
"hB
"NB
"NB
"�B
"�B
"�B
"�B
#�B
#�B
$&B
$@B
$�B
%B
%zB
&2B
&2B
&2B
&B
&fB
&�B
&�B
&�B
'RB
'�B
(�B
(�B
)*B
(�B
(�B
(�B
)�B
*KB
*B
*�B
+kB
+�B
+�B
+�B
,�B
-CB
-wB
-wB
-�B
.cB
.�B
.�B
.�B
/iB
/OB
0B
0;B
0UB
0;B
0�B
1'B
1[B
1�B
1�B
1�B
1�B
2-B
2B
2aB
3�B
3�B
3hB
3hB
4B
4�B
5�B
6FB
6�B
6�B
7fB
7�B
7�B
88B
8�B
9>B
9XB
9rB
9rB
9�B
:B
:DB
:DB
:*B
:�B
:�B
;JB
;B
;�B
<B
<PB
<PB
<6B
<6B
<6B
<B
;B
;�B
;B
;B
;B
;�B
;�B
;dB
;JB
<6B
<�B
=�B
=�B
>wB
=�B
=�B
=�B
>B
>(B
>(B
>�B
>�B
>�B
>�B
?B
?�B
?�B
@�B
A;B
A;B
AUB
AoB
A�B
A�B
A�B
A�B
B�B
CB
C-B
CaB
CB
B�B
B�B
B�B
B�B
CGB
CaB
C{B
C�B
C�B
DMB
D�B
EB
ESB
E�B
E�B
F?B
FtB
F�B
F�B
F�B
F�B
G+B
G�B
HB
HKB
HfB
IB
IB
IRB
I�B
I�B
I�B
J#B
J=B
JXB
J=B
J�B
J�B
J�B
LB
L0B
LJB
L0B
LJB
LdB
L�B
M6B
M6B
M�B
M�B
M�B
M�B
N"B
N<B
NpB
N�B
OBB
OBB
O\B
O�B
PHB
P�B
P�B
P�B
QhB
Q�B
RB
R B
R B
RoB
R�B
R�B
R�B
R�B
SB
S@B
S�B
S�B
S�B
S�B
T,B
T{B
T�B
T�B
T�B
T�B
UgB
U�B
V�B
V�B
V�B
V�B
V�B
W
B
W?B
WsB
W�B
XyB
X_B
XyB
YB
YB
YKB
Y�B
Y�B
ZB
Z7B
Z7B
Z�B
[	B
[	B
[=B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]IB
]�B
^B
^B
^B
]�B
^�B
^�B
^�B
_B
_B
_;B
_pB
_�B
`BB
`�B
`�B
aHB
a|B
abB
a|B
a�B
a�B
bhB
b�B
b�B
b�B
c B
cnB
c�B
c�B
c�B
d&B
dZB
d�B
d�B
d�B
d�B
d�B
eB
e,B
e`B
e`B
ezB
e�B
e�B
fB
ffB
f�B
f�B
f�B
f�B
f�B
gRB
g�B
h$B
h>B
h$B
hXB
h�B
h�B
h�B
h�B
iB
iDB
iyB
i�B
i�B
i�B
i�B
jB
j0B
jeB
j�B
jB
j�B
j�B
j�B
j�B
kB
kB
kQB
k6B
k�B
k�B
k�B
k�B
l"B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
mB
mwB
m�B
nIB
n�B
oB
oB
o5B
oiB
o�B
o�B
o�B
p!B
poB
p�B
p�B
p�B
p�B
p�B
qAB
q�B
q�B
q�B
q�B
rB
r-B
rGB
raB
raB
r�B
r�B
s3B
sMB
sMB
shB
s�B
s�B
s�B
s�B
s�B
tB
tTB
tnB
t�B
t�B
uB
u%B
u%B
u?B
u?B
uZB
u�B
u�B
u�B
u�B
v+B
vFB
v`B
v�B
v�B
v�B
wfB
w�B
xB
xB
xB
xB
xRB
xRB
x�B
x�B
x�B
yXB
yXB
yrB
yrB
y�B
y�B
zDB
z�B
z�B
z�B
{0B
{�B
{�B
|B
|PB
|�B
|�B
|�B
|�B
}B
}"B
}qB
}�B
}�B
}�B
}�B
~B
~]B
~BB
~�B
~�B
~�B
~�B
~�B
B
HB
HB
cB
}B
�B
�B
�B
�B
�4B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
�B
� B
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	A�B	A�B	A�B	AoB	A�B	A�B	A B	@�B	A;B	@�B	@B	?HB	=�B	6`B	0�B	/OB	.�B	.�B	/B	.cB	,�B	+�B	*�B	+B	,�B	2B	>�B	^�B	l�B	kkB	pUB	y$B	�XB	��B	z�B	�kB	�(B	��B	��B	��B	�UB
9�B
`B
sB
z*B
v�B
{�B
��B
ˬB
��B
��B
�B
�'B�B�B�B"�BA�BN�BUMBb4BlBt�B��B��B�B�1B�Bz�Bc�B}qB��B�Bs�BJ	B{B
�(B
�"B�B�B%`B �B	�B
�oB
��B
�RB
�vB
�B
��B
�HB
��B
zxB
$ZB	��B	��B	��B	�>B	�bB	{JB	lWB	W�B	C-B	33B	*�B	�B	�B	�B	 �B�B�tB�B��B�qB��B�FB�8BބB�dB�8B��B��B�B�B�B�VB�B�aB�BخB�B��B�>B�_B��B�B�B��B�`B��B��B	 �B	�B	�B	mB	pB	#�B	$�B	%�B	(sB	1�B	7�B	9>B	=�B	E�B	LJB	P�B	V�B	gB	t�B	u?B	y$B	xRB	u�B	�AB	�B	��B	�B	�	B	�pB	��B	��B	��B	��B	��B	�B	�WB	��B	�B	�;B	�B	��B	�B	��B	��B	�yB	��B	��B	�9B	�ZB	��B	�0B	�lB	��B	��B	�[B	��B	��B	�fB	�B	�RB	�PB	�XB	�(B	��B	��B	�XB	�	B	��B	�3B	�!B	�/B	�IB	�;B	�/B	��B	��B	�"B	��B	��B	��B	�GB	�qB	�B	��B	�B	�B	�NB	��B	��B	�LB	�2B	�2B	�WB	��B	��B	��B	��B	��B	��B	�eB	��B	�MB	�0B	�"B	��B	�cB	�AB	͟B	�BB	�PB	�#B	�GB	��B	�EB	�B	�B	�3B	ªB	� B	�iB	��B	��B	�HB	��B	��B	�qB	��B	��B	��B	�cB	�UB	�'B	�uB	żB	�B	�"B	�}B	�hB	�NB	� B	ЗB	�oB	�B	�MB	�sB	��B	�QB	��B	�B	�WB	��B	�B	�jB	ބB	�jB	ބB	��B	�;B	�;B	�VB	�BB	�vB	��B	��B	��B	�|B	�4B	�B	�4B	�B	��B	� B	�B	�4B	��B	�B	�B	�B	�fB	��B	��B	�B	�B	��B	��B	��B	�B	�5B	�iB	�B	�OB	�5B	�5B	�5B	�B	�B	�IB	�/B	��B	�B	��B	�B	��B	��B	�|B	�|B	�aB	�aB	�aB	�GB	�-B	�aB	�B	�B	�hB	�B	�B	��B	��B	��B	��B	�9B	��B	�B	�B	�ZB	�tB	��B	�tB	��B	��B	��B	��B	�zB	�fB	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�XB	�B	�DB	�^B	��B	�0B	�0B	�PB	��B	�<B	��B	��B	�B	�wB	�B	�}B	��B	�cB	��B	�}B
  B	�cB	��B
 4B
 �B
�B
�B
�B
�B
�B
3B
�B
�B
�B
SB
�B
�B
gB
�B
�B
�B
_B
�B
_B
�B
B
+B
�B
�B
�B
fB
�B
KB
1B
�B
�B

#B
)B
^B
DB

�B

�B
�B
JB
�B
^B
B
^B

�B

�B
^B
�B
�B
6B
�B
�B
�B
�B
(B
BB
�B
bB
NB
�B
�B
:B
�B
�B
�B
�B
�B
�B
�B
�B
�B
1B
QB
�B
B
�B
B
dB
�B
B
�B
�B
�B
�B
�B
�B
 �B
 BB
 \B
 \B
 BB
 'B
!-B
!|B
!B
 BB
�B
�B
 vB
 �B
 �B
 �B
 �B
!-B
!|B
!�B
"B
"hB
"NB
"NB
"�B
"�B
"�B
"�B
#�B
#�B
$&B
$@B
$�B
%B
%zB
&2B
&2B
&2B
&B
&fB
&�B
&�B
&�B
'RB
'�B
(�B
(�B
)*B
(�B
(�B
(�B
)�B
*KB
*B
*�B
+kB
+�B
+�B
+�B
,�B
-CB
-wB
-wB
-�B
.cB
.�B
.�B
.�B
/iB
/OB
0B
0;B
0UB
0;B
0�B
1'B
1[B
1�B
1�B
1�B
1�B
2-B
2B
2aB
3�B
3�B
3hB
3hB
4B
4�B
5�B
6FB
6�B
6�B
7fB
7�B
7�B
88B
8�B
9>B
9XB
9rB
9rB
9�B
:B
:DB
:DB
:*B
:�B
:�B
;JB
;B
;�B
<B
<PB
<PB
<6B
<6B
<6B
<B
;B
;�B
;B
;B
;B
;�B
;�B
;dB
;JB
<6B
<�B
=�B
=�B
>wB
=�B
=�B
=�B
>B
>(B
>(B
>�B
>�B
>�B
>�B
?B
?�B
?�B
@�B
A;B
A;B
AUB
AoB
A�B
A�B
A�B
A�B
B�B
CB
C-B
CaB
CB
B�B
B�B
B�B
B�B
CGB
CaB
C{B
C�B
C�B
DMB
D�B
EB
ESB
E�B
E�B
F?B
FtB
F�B
F�B
F�B
F�B
G+B
G�B
HB
HKB
HfB
IB
IB
IRB
I�B
I�B
I�B
J#B
J=B
JXB
J=B
J�B
J�B
J�B
LB
L0B
LJB
L0B
LJB
LdB
L�B
M6B
M6B
M�B
M�B
M�B
M�B
N"B
N<B
NpB
N�B
OBB
OBB
O\B
O�B
PHB
P�B
P�B
P�B
QhB
Q�B
RB
R B
R B
RoB
R�B
R�B
R�B
R�B
SB
S@B
S�B
S�B
S�B
S�B
T,B
T{B
T�B
T�B
T�B
T�B
UgB
U�B
V�B
V�B
V�B
V�B
V�B
W
B
W?B
WsB
W�B
XyB
X_B
XyB
YB
YB
YKB
Y�B
Y�B
ZB
Z7B
Z7B
Z�B
[	B
[	B
[=B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]B
]IB
]�B
^B
^B
^B
]�B
^�B
^�B
^�B
_B
_B
_;B
_pB
_�B
`BB
`�B
`�B
aHB
a|B
abB
a|B
a�B
a�B
bhB
b�B
b�B
b�B
c B
cnB
c�B
c�B
c�B
d&B
dZB
d�B
d�B
d�B
d�B
d�B
eB
e,B
e`B
e`B
ezB
e�B
e�B
fB
ffB
f�B
f�B
f�B
f�B
f�B
gRB
g�B
h$B
h>B
h$B
hXB
h�B
h�B
h�B
h�B
iB
iDB
iyB
i�B
i�B
i�B
i�B
jB
j0B
jeB
j�B
jB
j�B
j�B
j�B
j�B
kB
kB
kQB
k6B
k�B
k�B
k�B
k�B
l"B
l=B
lqB
l�B
l�B
l�B
l�B
l�B
l�B
mB
mwB
m�B
nIB
n�B
oB
oB
o5B
oiB
o�B
o�B
o�B
p!B
poB
p�B
p�B
p�B
p�B
p�B
qAB
q�B
q�B
q�B
q�B
rB
r-B
rGB
raB
raB
r�B
r�B
s3B
sMB
sMB
shB
s�B
s�B
s�B
s�B
s�B
tB
tTB
tnB
t�B
t�B
uB
u%B
u%B
u?B
u?B
uZB
u�B
u�B
u�B
u�B
v+B
vFB
v`B
v�B
v�B
v�B
wfB
w�B
xB
xB
xB
xB
xRB
xRB
x�B
x�B
x�B
yXB
yXB
yrB
yrB
y�B
y�B
zDB
z�B
z�B
z�B
{0B
{�B
{�B
|B
|PB
|�B
|�B
|�B
|�B
}B
}"B
}qB
}�B
}�B
}�B
}�B
~B
~]B
~BB
~�B
~�B
~�B
~�B
~�B
B
HB
HB
cB
}B
�B
�B
�B
�B
�4B
�iB
��B
��B
��B
��B
��B
��B
��B
��B
�B
� B
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105228  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191309  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191309  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191309                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041317  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041317  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                