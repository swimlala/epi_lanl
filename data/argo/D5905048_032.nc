CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-08-27T21:35:55Z creation;2016-08-27T21:35:57Z conversion to V3.1;2019-12-19T08:28:09Z update;     
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tL   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �\   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �l   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �p   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �t   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20160827213555  20200116201516  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL                A   JA  I2_0577_032                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @��s?�� 1   @��s�$�@40�d��8�d�[W>�61   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D���D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @mp�@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸B���B�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?��CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D�qDs�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�6�D�v�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�6�D�v�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�}D���D���D�9�D�y�D���D���D�9�D�}D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D��D�9�D�y�D��D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D춸D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�=D�v�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��A��A��A��A��A��A��A㝲A��A�7A�z�A�-A��A�
=A���A���A�hA��TA�;dA�A���Aޝ�Aޕ�A�ffA���AܓuA��mAۏ\A�S�Aڕ�A���A�XA��yA�+A�JAҟ�A�5?A�ƨAЅA���Aΰ!A�A�A��A�r�A̍PA�A���A˴9A�jAʝ�AɓuAȕ�A��
A�{A¡�A�p�A�ZA���A��uA�ĜA��
A���A��hA�$�A��A��TA���A��A�O�A�~�A�?}A���A��A��HA��DA��A�z�A� �A�`BA��
A�XA���A���A�A��A�  A�(�A�O�A�?}A�(�A�v�A��A��\A�Q�A�oA�M�A�G�A���A��`A��wA��A��`A���A�t�A��^A�oA���A��A��jA�bNA��yA�n�A���A�
=A�/A���A�n�A�VA�p�A�{A�A}��A}C�A|��A{��A{p�Ay�Ax�AwVAu7LAtE�As��AsoAr�!Aq�#Am�^AjVAghsAe/AcXA`�/A`ZA_hsA]�PA[��AXĜAV��ARM�AQ
=AO�AN�9AL�AJ�AIC�AH�AF5?AE�ACO�A@jA>��A=��A=�7A<  A:-A8�A8$�A7�A6bNA5ƨA4ffA2��A1�7A0��A/��A.VA,ȴA+�;A+K�A*1'A(��A'oA%�;A%7LA$��A"��A!x�A 1'A|�A�uA1'A��AĜA=qA  Ar�A�7A�/A�\A�FAbNA��A�\A��A�mAC�AhsAt�AA
�A
Q�A	��A�!A��A��AC�Ap�A�DAA�A M�@��@�ff@��#@�1@�/@���@���@�x�@��`@�F@���@��@�@� �@��H@陚@���@�u@�Z@��@��@�&�@�l�@�!@��@��@ޏ\@� �@�5?@�dZ@�E�@��@�33@�v�@�J@�/@�Z@�S�@�@�~�@��T@�5?@ؼj@�"�@��y@֧�@�J@�%@�1@�"�@��T@Л�@�A�@Ͼw@�"�@�J@͡�@�O�@̬@��@��
@��m@˶F@��H@�v�@�J@ɺ^@��@�z�@�Z@Ǿw@���@Ƨ�@Ƈ+@�{@�X@��/@Ĭ@�z�@� �@��;@�|�@�
=@��@�G�@�/@�Ĝ@�I�@���@��;@�ƨ@�\)@���@���@���@�j@�ƨ@�@�M�@��@�@�V@��@�X@�`B@��y@��y@�ȴ@�E�@�-@��T@�`B@��m@���@���@��9@���@��@��R@���@��+@��+@�hs@�Ĝ@�(�@�33@��!@���@��@�$�@���@��j@���@�Z@�Q�@�Z@�9X@��@��F@��P@��F@��F@��P@�
=@�o@�o@��y@��!@�$�@���@�p�@���@��u@�9X@�o@��R@�V@�-@���@�/@��@��@�I�@�j@�1@�  @�\)@�"�@�E�@���@�dZ@�1@�1@���@�\)@�33@���@�=q@��@��^@���@���@���@���@�O�@���@�Ĝ@��u@��u@�r�@�9X@�(�@�b@�ƨ@��@���@�C�@�@��H@�n�@�@�`B@�7L@���@��j@��D@�bN@�I�@��@�ƨ@��@�C�@��y@�ȴ@�ȴ@���@��\@�M�@��@��@��#@��-@��7@��@��u@��D@�z�@�j@�I�@�b@��
@�ƨ@��F@��@�ȴ@���@���@�~�@�^5@�@�?}@�&�@��@��@��9@�A�@�b@��w@�C�@��y@�v�@�M�@�-@�J@���@�x�@�`B@��@�Ĝ@�r�@�Z@�I�@�1'@�1@��w@�l�@�;d@��y@���@��\@�v�@�n�@�ff@�M�@��@���@��@�?}@�&�@��@��/@�j@�  @��
@��F@���@��@�t�@�dZ@�dZ@��m@��@�b@��@�\)@��@���@��+@�M�@�p�@��j@��@��m@��F@�dZ@�33@�
=@��\@�E�@�-@�$�@��@�$�@��@��@��@�{@���@��@��-@���@���@�?}@�V@��`@��u@�r�@�j@�bN@�b@���@�o@��R@�v�@�V@�=q@��#@��7@�hs@�?}@��`@���@��@�j@�Z@�Q�@� �@�P@~��@~�+@~ff@}�T@}?}@|�/@|j@{�m@{��@{��@{dZ@{dZ@{"�@z��@zn�@z-@y��@y��@x��@x1'@x  @w�P@v��@v�R@vV@v{@u�T@up�@u/@tz�@s�m@s��@sC�@r�H@rn�@q��@p��@p�9@o�;@o+@n�y@nȴ@n��@n�+@nff@nE�@m�T@mp�@l�@k�m@k�@kdZ@k@j�H@jM�@i�7@iG�@h�u@g�@gl�@f��@fV@f@e�T@e��@e��@e@e��@eO�@e/@d�/@d��@d1@c��@b�@a�@aG�@`�`@`�9@`bN@`A�@_�w@_
=@^V@^@]@]p�@\��@\��@\I�@\�@\1@\�@\1@[dZ@[S�@[33@Z�H@Z�!@Y�#@Y&�@X�`@X�u@Xr�@X1'@W�w@W�P@W
=@V�@V�R@V��@Vv�@VE�@U�@U��@U�@T��@T�D@Tj@TZ@TI�@TI�@T(�@T1@S��@S�
@S��@St�@SS�@S@R�\@RM�@RJ@Q�^@Q�7@Q7L@P�`@P�u@PbN@O�;@O|�@O;d@O+@O+@N��@N�+@NV@M�@M/@L��@L�@L�@L9X@J�@J~�@JM�@JM�@J=q@I�@I�^@Ix�@I�@HĜ@Hr�@H1'@G|�@G+@G�@G
=@F�@F��@F5?@F@E�T@E�T@E��@E`B@D��@D1@C�m@C�
@Cƨ@Ct�@B�!@B-@AG�@@��@@A�@@b@?�w@?l�@>��@>v�@=��@=?}@=�@<��@<��@<j@<1@;�m@;ƨ@;��@;t�@;C�@;33@;o@:�H@:��@9�@8��@8bN@7�@7|�@6ȴ@6ff@6E�@6{@5@5`B@5O�@5V@4��@4��@4�@3�
@3dZ@2��@2��@2^5@1��@1�7@17L@1�@0Ĝ@0bN@0b@/�@/;d@.��@.ff@.@-��@-�@,��@+��@+�F@+��@+S�@*��@*n�@*=q@*�@)�^@)x�@)%@(�u@(�@(Q�@(  @'l�@&ȴ@&ff@%�@%`B@%/@$��@$Z@$�@#��@#�
@#��@#t�@#S�@"��@"n�@"^5@"J@!��@!G�@ ��@ �`@ ��@ Ĝ@ �9@ �@ r�@ bN@ 1'@�w@+@�y@�+@V@5?@$�@�@@�h@?}@V@�@��@�D@j@I�@�m@dZ@C�@33@��@^5@M�@M�@M�@�@J@��@7L@�`@��@Ĝ@Ĝ@��@��@��@��@��@�u@r�@1'@b@  @�@�;@�;@��@��@��@��@�w@��@|�@�P@\)@;d@�y@�R@��@v�@{@�@�@V@��@��@I�@�@��@�m@�
@�F@��@��@��@��@�@�@t�@dZ@S�@C�@C�@C�@"�@o@o@�H@��@��@n�@^5@-@�@��@x�@X@7L@%@�`@��@�@�@r�@A�@ �@  @b@b@b@�;@|�@;d@+@�y@�R@�+@ff@E�@{@{@{@@�-@`B@?}@?}@?}@/@�@��@��@�@��@��@��@�D@�D@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��A��A��A��A��A��A��A��A��A��A㝲A��A�7A�z�A�-A��A�
=A���A���A�hA��TA�;dA�A���Aޝ�Aޕ�A�ffA���AܓuA��mAۏ\A�S�Aڕ�A���A�XA��yA�+A�JAҟ�A�5?A�ƨAЅA���Aΰ!A�A�A��A�r�A̍PA�A���A˴9A�jAʝ�AɓuAȕ�A��
A�{A¡�A�p�A�ZA���A��uA�ĜA��
A���A��hA�$�A��A��TA���A��A�O�A�~�A�?}A���A��A��HA��DA��A�z�A� �A�`BA��
A�XA���A���A�A��A�  A�(�A�O�A�?}A�(�A�v�A��A��\A�Q�A�oA�M�A�G�A���A��`A��wA��A��`A���A�t�A��^A�oA���A��A��jA�bNA��yA�n�A���A�
=A�/A���A�n�A�VA�p�A�{A�A}��A}C�A|��A{��A{p�Ay�Ax�AwVAu7LAtE�As��AsoAr�!Aq�#Am�^AjVAghsAe/AcXA`�/A`ZA_hsA]�PA[��AXĜAV��ARM�AQ
=AO�AN�9AL�AJ�AIC�AH�AF5?AE�ACO�A@jA>��A=��A=�7A<  A:-A8�A8$�A7�A6bNA5ƨA4ffA2��A1�7A0��A/��A.VA,ȴA+�;A+K�A*1'A(��A'oA%�;A%7LA$��A"��A!x�A 1'A|�A�uA1'A��AĜA=qA  Ar�A�7A�/A�\A�FAbNA��A�\A��A�mAC�AhsAt�AA
�A
Q�A	��A�!A��A��AC�Ap�A�DAA�A M�@��@�ff@��#@�1@�/@���@���@�x�@��`@�F@���@��@�@� �@��H@陚@���@�u@�Z@��@��@�&�@�l�@�!@��@��@ޏ\@� �@�5?@�dZ@�E�@��@�33@�v�@�J@�/@�Z@�S�@�@�~�@��T@�5?@ؼj@�"�@��y@֧�@�J@�%@�1@�"�@��T@Л�@�A�@Ͼw@�"�@�J@͡�@�O�@̬@��@��
@��m@˶F@��H@�v�@�J@ɺ^@��@�z�@�Z@Ǿw@���@Ƨ�@Ƈ+@�{@�X@��/@Ĭ@�z�@� �@��;@�|�@�
=@��@�G�@�/@�Ĝ@�I�@���@��;@�ƨ@�\)@���@���@���@�j@�ƨ@�@�M�@��@�@�V@��@�X@�`B@��y@��y@�ȴ@�E�@�-@��T@�`B@��m@���@���@��9@���@��@��R@���@��+@��+@�hs@�Ĝ@�(�@�33@��!@���@��@�$�@���@��j@���@�Z@�Q�@�Z@�9X@��@��F@��P@��F@��F@��P@�
=@�o@�o@��y@��!@�$�@���@�p�@���@��u@�9X@�o@��R@�V@�-@���@�/@��@��@�I�@�j@�1@�  @�\)@�"�@�E�@���@�dZ@�1@�1@���@�\)@�33@���@�=q@��@��^@���@���@���@���@�O�@���@�Ĝ@��u@��u@�r�@�9X@�(�@�b@�ƨ@��@���@�C�@�@��H@�n�@�@�`B@�7L@���@��j@��D@�bN@�I�@��@�ƨ@��@�C�@��y@�ȴ@�ȴ@���@��\@�M�@��@��@��#@��-@��7@��@��u@��D@�z�@�j@�I�@�b@��
@�ƨ@��F@��@�ȴ@���@���@�~�@�^5@�@�?}@�&�@��@��@��9@�A�@�b@��w@�C�@��y@�v�@�M�@�-@�J@���@�x�@�`B@��@�Ĝ@�r�@�Z@�I�@�1'@�1@��w@�l�@�;d@��y@���@��\@�v�@�n�@�ff@�M�@��@���@��@�?}@�&�@��@��/@�j@�  @��
@��F@���@��@�t�@�dZ@�dZ@��m@��@�b@��@�\)@��@���@��+@�M�@�p�@��j@��@��m@��F@�dZ@�33@�
=@��\@�E�@�-@�$�@��@�$�@��@��@��@�{@���@��@��-@���@���@�?}@�V@��`@��u@�r�@�j@�bN@�b@���@�o@��R@�v�@�V@�=q@��#@��7@�hs@�?}@��`@���@��@�j@�Z@�Q�@� �@�P@~��@~�+@~ff@}�T@}?}@|�/@|j@{�m@{��@{��@{dZ@{dZ@{"�@z��@zn�@z-@y��@y��@x��@x1'@x  @w�P@v��@v�R@vV@v{@u�T@up�@u/@tz�@s�m@s��@sC�@r�H@rn�@q��@p��@p�9@o�;@o+@n�y@nȴ@n��@n�+@nff@nE�@m�T@mp�@l�@k�m@k�@kdZ@k@j�H@jM�@i�7@iG�@h�u@g�@gl�@f��@fV@f@e�T@e��@e��@e@e��@eO�@e/@d�/@d��@d1@c��@b�@a�@aG�@`�`@`�9@`bN@`A�@_�w@_
=@^V@^@]@]p�@\��@\��@\I�@\�@\1@\�@\1@[dZ@[S�@[33@Z�H@Z�!@Y�#@Y&�@X�`@X�u@Xr�@X1'@W�w@W�P@W
=@V�@V�R@V��@Vv�@VE�@U�@U��@U�@T��@T�D@Tj@TZ@TI�@TI�@T(�@T1@S��@S�
@S��@St�@SS�@S@R�\@RM�@RJ@Q�^@Q�7@Q7L@P�`@P�u@PbN@O�;@O|�@O;d@O+@O+@N��@N�+@NV@M�@M/@L��@L�@L�@L9X@J�@J~�@JM�@JM�@J=q@I�@I�^@Ix�@I�@HĜ@Hr�@H1'@G|�@G+@G�@G
=@F�@F��@F5?@F@E�T@E�T@E��@E`B@D��@D1@C�m@C�
@Cƨ@Ct�@B�!@B-@AG�@@��@@A�@@b@?�w@?l�@>��@>v�@=��@=?}@=�@<��@<��@<j@<1@;�m@;ƨ@;��@;t�@;C�@;33@;o@:�H@:��@9�@8��@8bN@7�@7|�@6ȴ@6ff@6E�@6{@5@5`B@5O�@5V@4��@4��@4�@3�
@3dZ@2��@2��@2^5@1��@1�7@17L@1�@0Ĝ@0bN@0b@/�@/;d@.��@.ff@.@-��@-�@,��@+��@+�F@+��@+S�@*��@*n�@*=q@*�@)�^@)x�@)%@(�u@(�@(Q�@(  @'l�@&ȴ@&ff@%�@%`B@%/@$��@$Z@$�@#��@#�
@#��@#t�@#S�@"��@"n�@"^5@"J@!��@!G�@ ��@ �`@ ��@ Ĝ@ �9@ �@ r�@ bN@ 1'@�w@+@�y@�+@V@5?@$�@�@@�h@?}@V@�@��@�D@j@I�@�m@dZ@C�@33@��@^5@M�@M�@M�@�@J@��@7L@�`@��@Ĝ@Ĝ@��@��@��@��@��@�u@r�@1'@b@  @�@�;@�;@��@��@��@��@�w@��@|�@�P@\)@;d@�y@�R@��@v�@{@�@�@V@��@��@I�@�@��@�m@�
@�F@��@��@��@��@�@�@t�@dZ@S�@C�@C�@C�@"�@o@o@�H@��@��@n�@^5@-@�@��@x�@X@7L@%@�`@��@�@�@r�@A�@ �@  @b@b@b@�;@|�@;d@+@�y@�R@�+@ff@E�@{@{@{@@�-@`B@?}@?}@?}@/@�@��@��@�@��@��@��@�D@�D@�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B	7B\B�B@�BC�BC�B:^B1'B+B)�B�B
�5B
B
�B
�B
�-B
�dB
��B
��B
��B
��B
�B
�HB
��B%�B;dBB�BH�BT�BjB� B�+B�{B�B�dB��B�B8RBz�BɺBBBBuB�B�B�B�B�B�BJBJBDB
=BB�B�B�`B�NB�5B�)B�B�B��B��B��BǮB��B�3B��B��B�JB�1B�B�BgmB�B��B�=BT�B?}B5?B@�BD�B;dB7LB1'B/B+B�BbBJBB
��B
�sB
��B
�qB
�FB
��B
�DB
x�B
m�B
gmB
cTB
]/B
XB
O�B
D�B
=qB
/B
$�B
�B
�B
�B
!�B
hB	�B	�NB	��B	ÖB	�LB	�B	��B	��B	�\B	� B	s�B	XB	O�B	H�B	A�B	9XB	,B	#�B	�B	�B	bB	+B��B�B�B�B�`B�/B�B��B��B��B��BǮB��B�^B�RB�3B�'B�B��B��B��B��B��B��B�{B�uB�hB�VB�DB�=B�1B�+B�%B�B�B�B� B|�Bz�By�Bx�Bu�Br�Bp�Bl�Bk�BjBiyBffB`BB\)BYBYBXBXB[#B\)B]/B]/BZB_;BcTBbNBbNBcTBe`Be`BcTBcTBe`BcTBe`Be`BgmBgmBiyBjBjBk�Bk�Bk�Bk�Bp�Bu�B~�B�B�DB�\B�PB��B�FBBŢBB��B��B��BǮB��B��B��B��B�
B�;B�HB�)B�ZB�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	%B	+B		7B	
=B	PB	VB	\B	\B	hB	oB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	$�B	)�B	/B	0!B	33B	7LB	9XB	:^B	:^B	;dB	<jB	A�B	E�B	F�B	H�B	I�B	I�B	J�B	K�B	P�B	R�B	VB	[#B	k�B	m�B	n�B	n�B	n�B	n�B	m�B	jB	iyB	iyB	hsB	k�B	q�B	�B	�+B	�PB	�hB	�bB	�\B	�VB	�\B	�JB	�JB	�7B	�B	}�B	|�B	}�B	�B	�B	�%B	�+B	�+B	�1B	�PB	�\B	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�LB	�jB	��B	�wB	�}B	�wB	ÖB	��B	��B	�
B	�B	�B	�B	�/B	�/B	�5B	�NB	�TB	�TB	�TB	�ZB	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
1B
1B
1B

=B
	7B
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
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
VB
VB
VB
PB
PB
JB
JB
JB
JB
PB
PB
hB
bB
hB
hB
bB
\B
VB
VB
VB
JB
DB

=B
	7B

=B
DB
DB
JB
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
bB
bB
bB
hB
oB
oB
uB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
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
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
,B
,B
,B
,B
,B
-B
-B
.B
.B
/B
/B
/B
0!B
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
2-B
2-B
2-B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
5?B
5?B
6FB
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
9XB
9XB
:^B
:^B
:^B
:^B
;dB
;dB
<jB
<jB
=qB
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
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
>wB
>wB
>wB
?}B
?}B
@�B
@�B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
C�B
C�B
C�B
C�B
C�B
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
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
K�B
J�B
J�B
K�B
K�B
K�B
L�B
L�B
M�B
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
N�B
O�B
O�B
P�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
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
YB
ZB
ZB
ZB
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
]/B
]/B
^5B
^5B
_;B
_;B
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
bNB
bNB
bNB
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
hsB
iyB
iyB
iyB
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
n�B
n�B
n�B
o�B
o�B
p�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
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
v�B
v�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
��B
��B
�B
�B
�B
�B
�B
��B
�B
�B
�B
�B
�6B
�B
�dB
�*B
�*B
�0B
�}B
��B
�B
��B
��BUB	�B�B �BB�BD�BD�B;JB2aB-]B.IB%`B
�-B
��B
��B
��B
��B
��B
ˬB
�jB
��B
��B
�
B
�B
��B&�B;�BC-BI�BW$Bl�B��B��B��B��B�B��B�?B9$B|�B�^BUB{B1B�B_BKBEBBkB�B�B�BPB�B�B��B�)B�B�B�pB�/B�WB�kB�uB�jB��B��B�3B�B�B��B�PB�RB�+B��Bn}B'RBیB��BW�BA�B88BCGBF?B=B8�B3B2GB-�B�B�B�BB
�lB
�B
՛B
�}B
��B
�B
�<B
z�B
n�B
h$B
dZB
^OB
ZB
Q�B
F�B
?}B
0UB
%�B
�B
�B
!�B
&fB
gB	�?B	�,B	�[B	�?B	�lB	��B	�sB	�B	��B	�aB	xB	ZB	Q�B	J�B	D3B	<PB	-�B	%�B	!-B	yB	B	
rB��B��B��B��B�B��B�eB�SB�&B�(B��B��B�'B��B��B�%B�B�WB�$B��B��B��B�=B��B��B��B�uB��B�dB�xB�B�B��B�B�B��B�UB}�B{�B{dBz�BxRBuBq�BmCBl�Bm)BlBh>Ba�B]/BZBZ�BY�BZB]IB^jB^�B^5BZ�BaBdtBb�Bc:Bd�Bg8Bf�Bd&BdtBfBd�Bf�BgBh�Bh�BjBkkBkBk�BlBl=Bl�Bq�Bv�B�B��B�JB�HB��B��B�BÖB��B�aB�'B�B�UB�fB̈́B�.B�hBՁB�sB�\B�NBܬB��B�B�wB�oB�|B��B��B�DB�B��B��B	 iB	oB	�B	�B	tB	EB		�B	
�B	�B	�B	�B	�B	�B	�B	B	B	�B	�B	#B	CB	B	 B	 B	 'B	!-B	"NB	#nB	%�B	*�B	/iB	0�B	3�B	7�B	9�B	:�B	:�B	<B	=VB	B'B	F%B	GEB	IlB	JXB	J#B	KB	LJB	Q B	R�B	U�B	Z�B	k�B	m�B	o B	n�B	o B	oOB	n�B	k�B	jeB	j0B	h�B	j�B	p�B	�MB	�_B	��B	�:B	� B	��B	�B	��B	��B	��B	��B	��B	~BB	}<B	~BB	�'B	�MB	�YB	�_B	�zB	�KB	�PB	��B	��B	��B	��B	��B	��B	��B	�5B	�B	�BB	�NB	�TB	�ZB	��B	�>B	�_B	�KB	�B	�qB	�]B	�}B	�[B	�TB	��B	��B	�B	��B	� B	�BB	�GB	˒B	�B	�YB	ؓB	�kB	ڠB	ݲB	�~B	ޞB	�B	�nB	�TB	�B	�B	��B	�B	�B	�B	�B	��B	�B	��B	��B	��B	�B	� B	��B	��B	�B	�!B	�B	��B	��B	��B	�B	��B	��B	�B	�B	��B	�B	�B	�B	��B	�B	�B	�$B	�$B	�B	�B	�*B	�*B	�^B	�<B	�"B	�"B	�"B	�B	�"B	�<B	�B	�BB	�wB
 4B
;B
;B
;B
UB
oB
�B
MB
MB
gB
gB
�B
mB
�B
�B
�B
�B
_B
_B
_B
zB
�B
�B
�B

rB
	lB
xB
xB
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
pB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
jB
~B
~B
~B
JB
PB
6B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B

rB
	�B

�B
�B
�B
�B
�B
�B
�B
vB
vB
vB
\B
vB
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
	B
�B
�B
�B
�B
�B
B
�B
�B
 �B
 �B
 �B
 �B
!B
!�B
!�B
!�B
# B
# B
# B
#B
# B
$&B
$B
$&B
%B
%B
%,B
%,B
%FB
&2B
&2B
'B
'8B
'8B
'RB
'RB
'B
(XB
(XB
)*B
)*B
)*B
)*B
)*B
*0B
*KB
*KB
*eB
,WB
,"B
,=B
,=B
,=B
-]B
-wB
.cB
.}B
/�B
/OB
/iB
0oB
0UB
1[B
1AB
1AB
1[B
1[B
1[B
1AB
1[B
2aB
2|B
2|B
2�B
3�B
4�B
4nB
4nB
4nB
4nB
4�B
4�B
4�B
4�B
4nB
4nB
4�B
4nB
4nB
4nB
5ZB
5ZB
5tB
6�B
7�B
7�B
8lB
8�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
;B
;�B
<�B
<�B
=�B
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
>�B
>�B
>�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
>�B
>�B
>�B
?�B
?�B
@�B
@�B
?�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
BB
C�B
C�B
C�B
C�B
C�B
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
IB
IB
I�B
I�B
I�B
J	B
J#B
J	B
J#B
KB
K�B
J�B
J�B
K�B
LB
LB
MB
M6B
NB
MB
NB
NB
NB
M�B
NB
NB
NB
OB
OB
N�B
OB
O(B
OBB
PHB
P.B
Q4B
Q4B
R:B
R B
S&B
SB
S&B
S&B
SB
S&B
S&B
SB
S@B
T,B
TFB
UMB
U2B
U2B
UMB
U2B
V9B
V9B
V9B
VSB
VSB
W?B
W?B
W$B
WYB
XEB
XEB
X_B
X_B
YKB
Z7B
ZQB
ZkB
[qB
[WB
[WB
[WB
[=B
\CB
\xB
]dB
]dB
]dB
]~B
]~B
^�B
^�B
_�B
_�B
`vB
`�B
`vB
`vB
`vB
a|B
a|B
a|B
a|B
a|B
a|B
b�B
b�B
b�B
c�B
c�B
dtB
d�B
ezB
ezB
e�B
ezB
e�B
e�B
e�B
f�B
f�B
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
j�B
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
n�B
n�B
n�B
o�B
o�B
p�B
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
s�B
s�B
t�B
t�B
t�B
t�B
t�B
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
v�B
v�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.19(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201609010034362016090100343620160901003436201806221301232018062213012320180622130123201804050700312018040507003120180405070031  JA  ARFMdecpA19c                                                                20160828063506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20160827213555  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20160827213555  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20160827213556  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20160827213557  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20160827213557  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20160827213557  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20160827213557  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20160827213557  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20160827213557                      G�O�G�O�G�O�                JA  ARUP                                                                        20160827222427                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20160828153738  CV  JULD            G�O�G�O�F�3�                JM  ARCAJMQC2.0                                                                 20160831153436  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20160831153436  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404220031  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622040123  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116201516                      G�O�G�O�G�O�                