CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-01T21:35:12Z creation;2018-01-01T21:35:16Z conversion to V3.1;2019-12-19T07:52:48Z update;     
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
_FillValue                 �  IL   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tp   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �D   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180101213512  20200115121517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_195                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�As<� 1   @�As��J @;]�E���dW~���$1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D���D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�D�&f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�Q�@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�v�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�=D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�}D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�6�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D��D� R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A��A��A��!A��!A��-A��A��A��A���A���A��hA�r�A�I�A��yA���A���A�bA���A�r�A�z�A���A��A�{A���A��A�n�A�p�A��RA�\)A��A�hsA�|�A��A�JA�7LA�?}A�hsA���A�%A��-A��A��A��
A�33A�hsA��#A�\)A���A�dZA���A��A��\A���A�
=A��A�"�A��#A���A�{A�XA���A��DA��TA��A~��A}x�A|  A{VAzAx�/Ax�\Ax-Aw�Av~�AuXAs�;AsdZAr�!ArbAq�Ao�Amp�Al�RAl$�Ak��AkC�Aj�`Ajn�Ai�TAi�7Ait�Ai`BAi�Ahz�AgAg�hAf�AdȴAd��AdVAa��A`�!A^��A]p�A\�A\ �A[t�AZĜAZQ�AZ-AZbAY��AY/AW�#AVffAU�PAUAS|�ARr�AQ��AP�HAOK�AM�#AM�AL�yAL�jALbNAK�AI�#AH��AH~�AH$�AFn�AD��AC��ACp�ACVAB�AA��AAl�A?�#A>A�A>JA<��A;��A;�A;�#A;�
A;ƨA;x�A: �A9;dA7��A5G�A3��A3S�A3&�A2�yA2�A2M�A2�A1��A1G�A0A/&�A.�A.�!A-��A-O�A,$�A*�\A)�PA)+A(�A(ȴA(��A(M�A'�A%�7A#�mA"5?A!��A v�A�A/A��A�A%A��AI�A��A�hA�7A�7A�A�AhsAS�A33A&�A�HA��AVAhsA5?A�
A��A��A��AS�An�A��AK�A�\A�^A?}A1'A�7A\)A"�A
ȴA
I�A	�PA��A��A�-A;dA�A  A��A��A��AjA{A�A�A M�@�\)@��@��@��u@��@�33@��@��@�C�@�K�@��@��@��m@��@�R@�M�@���@���@��@�?}@�A�@�t�@�M�@�@�@�$�@ݑh@܋D@�|�@��@��T@�Q�@���@�V@���@�1@��;@�33@��T@�/@ЋD@��@��@�-@�%@�b@�@���@�33@�;d@���@��9@�b@�C�@��H@���@���@���@��R@��!@��R@���@���@���@���@��@�A�@��w@�\)@�=q@�p�@�7L@��9@��F@�E�@��7@���@�n�@���@�1'@���@��@��R@��@�p�@�&�@�V@��D@�  @��w@�t�@�o@�ff@��^@��@��D@��@�@��@�x�@�O�@�/@�bN@��@��+@�=q@��@�7L@�V@��@�Ĝ@��D@�Q�@�  @���@�t�@�"�@��@�ff@���@�&�@��@��D@�j@�A�@�1@���@�;d@��y@�o@��H@�-@�O�@��m@��+@�^5@�-@�{@��#@���@�x�@�?}@��@���@��
@�K�@�~�@���@�z�@�ƨ@�33@�ȴ@�~�@�@��-@��@�&�@��`@���@�1'@�|�@��y@�V@���@��@��#@���@�/@�bN@���@�ȴ@���@�E�@���@�@���@��@�`B@�?}@�&�@�%@��@���@�9X@��P@�33@�@���@���@���@�v�@�n�@�E�@�O�@���@���@�j@�1'@�1@�w@|�@;d@~�R@~V@}�@}�h@}`B@|�j@|9X@{C�@z�H@z~�@z-@y��@y%@xĜ@xĜ@xr�@xb@w�P@wK�@v��@v{@u`B@tz�@s�@s��@r�\@q�^@q��@q�@rJ@qG�@q&�@p�@o��@o;d@o
=@n��@n��@nv�@n��@oK�@o;d@nV@m�-@m��@m/@l��@l�@lZ@l1@l(�@l1@k��@k��@k��@k�m@k�
@kƨ@k�F@k��@ko@j�H@j�H@j�H@j�@j�@j�@j�H@j�H@j�@k@j��@j��@j��@j-@i7L@i&�@i&�@ix�@iG�@i%@hĜ@hĜ@h�u@h�u@h1'@hb@g�@g|�@g;d@g
=@fff@f5?@e�@e��@eO�@d�/@dz�@d�D@d�@cS�@co@b�@b�H@b�!@b��@b^5@b-@a��@`Ĝ@`r�@_�@_��@_+@^�R@^v�@^V@^E�@]@]�@]/@\�@\�D@\j@[�F@[t�@Z��@Z=q@Z�@ZJ@Y�#@Y�^@Yx�@Y7L@X�9@XQ�@XQ�@XA�@W�@W��@W�P@W;d@W+@W
=@V�y@V��@Vv�@VV@U�@U��@U�@T�/@Tj@T1@S��@SC�@R�!@QG�@PĜ@P��@P�u@Pr�@P �@O�@O�w@O�P@Ol�@O
=@N�@N�+@N5?@M�h@M/@L��@L�/@L�D@K�m@KdZ@J��@J�\@JM�@Ix�@I7L@H�@HA�@G�@G�w@G|�@G�@F�R@F��@FV@F5?@F$�@F{@F@E�@E�T@E��@D��@D��@Dj@D(�@C��@C�
@C�
@Cƨ@Cƨ@C�F@C��@CC�@B�!@B�@A�#@A�^@A��@Ax�@A&�@@��@@�9@@�@@A�@@1'@@ �@?�@?��@?��@?�@?l�@?+@?
=@>�@>E�@>{@=�T@=��@=/@=V@=V@<�/@<�@<Z@<I�@<I�@<9X@<9X@<I�@<(�@;��@;�m@;�m@;�m@;�
@;��@;C�@;33@;33@;o@:�@:��@:~�@:^5@:-@9��@9x�@9%@8�`@8Ĝ@8��@81'@7�@7��@7�@7|�@7+@7
=@6��@6�y@6�R@6v�@6�+@6�+@6E�@5@4�@4�@4j@4�@3ƨ@3�@3dZ@333@3"�@3o@3o@2�@2�H@2��@2�!@2M�@2J@1�^@17L@0��@0�u@0 �@/�@/�@/�;@/�w@/�@/�P@/K�@/�@.��@.�R@.��@.�+@.V@.$�@-�@-V@,��@,�D@,z�@,I�@+�
@+C�@*��@*~�@*^5@*M�@*J@)�^@)x�@)hs@)X@)7L@(��@(�`@(�@(1'@(b@(  @'�@'�w@'�P@'\)@'+@'
=@&�y@&��@&��@&5?@%@%�h@%?}@$��@$�@$Z@$(�@$�@$1@#�
@#�F@#dZ@#o@"��@"��@"��@"�\@!�#@!x�@!X@!&�@ ��@ bN@ b@   @��@l�@
=@�y@�R@��@��@�+@ff@{@��@��@?}@��@�@j@�m@�F@�F@�F@�@S�@C�@"�@�H@�\@~�@^5@-@�@��@7L@��@r�@bN@A�@1'@  @�@�P@l�@\)@\)@\)@K�@��@�@�+@@��@O�@�/@�/@��@�@j@9X@�@�m@C�@��@�\@~�@^5@J@��@��@x�@hs@X@7L@%@��@�9@�u@Q�@b@�@�@��@�@��@�P@|�@l�@l�@+@
=@��@�@v�@V@5?@5?@{@{@{@{@{@{@@�T@�-@�h@p�@O�@��@I�@��@ƨ@�@t�@C�@
�@
^5@
J@	��@	�@	�#@	�#@	�#@	��@	��@	�^@	�^@	��@	�7@	�7@	X@	X@	7L@	�@	%@��@��@Ĝ@Ĝ@Ĝ@�9@�@Q�@1'@�@�w@�P@l�@K�@;d@��@�@�+@@@�-@�h@O�@�j@j@Z@9X@1@��@�m@�F@��@S�@o@�@��@�!@�!@��@��@�\@n�@n�@n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���A���A���A���A���A���A���A��A��A��!A��!A��-A��A��A��A���A���A��hA�r�A�I�A��yA���A���A�bA���A�r�A�z�A���A��A�{A���A��A�n�A�p�A��RA�\)A��A�hsA�|�A��A�JA�7LA�?}A�hsA���A�%A��-A��A��A��
A�33A�hsA��#A�\)A���A�dZA���A��A��\A���A�
=A��A�"�A��#A���A�{A�XA���A��DA��TA��A~��A}x�A|  A{VAzAx�/Ax�\Ax-Aw�Av~�AuXAs�;AsdZAr�!ArbAq�Ao�Amp�Al�RAl$�Ak��AkC�Aj�`Ajn�Ai�TAi�7Ait�Ai`BAi�Ahz�AgAg�hAf�AdȴAd��AdVAa��A`�!A^��A]p�A\�A\ �A[t�AZĜAZQ�AZ-AZbAY��AY/AW�#AVffAU�PAUAS|�ARr�AQ��AP�HAOK�AM�#AM�AL�yAL�jALbNAK�AI�#AH��AH~�AH$�AFn�AD��AC��ACp�ACVAB�AA��AAl�A?�#A>A�A>JA<��A;��A;�A;�#A;�
A;ƨA;x�A: �A9;dA7��A5G�A3��A3S�A3&�A2�yA2�A2M�A2�A1��A1G�A0A/&�A.�A.�!A-��A-O�A,$�A*�\A)�PA)+A(�A(ȴA(��A(M�A'�A%�7A#�mA"5?A!��A v�A�A/A��A�A%A��AI�A��A�hA�7A�7A�A�AhsAS�A33A&�A�HA��AVAhsA5?A�
A��A��A��AS�An�A��AK�A�\A�^A?}A1'A�7A\)A"�A
ȴA
I�A	�PA��A��A�-A;dA�A  A��A��A��AjA{A�A�A M�@�\)@��@��@��u@��@�33@��@��@�C�@�K�@��@��@��m@��@�R@�M�@���@���@��@�?}@�A�@�t�@�M�@�@�@�$�@ݑh@܋D@�|�@��@��T@�Q�@���@�V@���@�1@��;@�33@��T@�/@ЋD@��@��@�-@�%@�b@�@���@�33@�;d@���@��9@�b@�C�@��H@���@���@���@��R@��!@��R@���@���@���@���@��@�A�@��w@�\)@�=q@�p�@�7L@��9@��F@�E�@��7@���@�n�@���@�1'@���@��@��R@��@�p�@�&�@�V@��D@�  @��w@�t�@�o@�ff@��^@��@��D@��@�@��@�x�@�O�@�/@�bN@��@��+@�=q@��@�7L@�V@��@�Ĝ@��D@�Q�@�  @���@�t�@�"�@��@�ff@���@�&�@��@��D@�j@�A�@�1@���@�;d@��y@�o@��H@�-@�O�@��m@��+@�^5@�-@�{@��#@���@�x�@�?}@��@���@��
@�K�@�~�@���@�z�@�ƨ@�33@�ȴ@�~�@�@��-@��@�&�@��`@���@�1'@�|�@��y@�V@���@��@��#@���@�/@�bN@���@�ȴ@���@�E�@���@�@���@��@�`B@�?}@�&�@�%@��@���@�9X@��P@�33@�@���@���@���@�v�@�n�@�E�@�O�@���@���@�j@�1'@�1@�w@|�@;d@~�R@~V@}�@}�h@}`B@|�j@|9X@{C�@z�H@z~�@z-@y��@y%@xĜ@xĜ@xr�@xb@w�P@wK�@v��@v{@u`B@tz�@s�@s��@r�\@q�^@q��@q�@rJ@qG�@q&�@p�@o��@o;d@o
=@n��@n��@nv�@n��@oK�@o;d@nV@m�-@m��@m/@l��@l�@lZ@l1@l(�@l1@k��@k��@k��@k�m@k�
@kƨ@k�F@k��@ko@j�H@j�H@j�H@j�@j�@j�@j�H@j�H@j�@k@j��@j��@j��@j-@i7L@i&�@i&�@ix�@iG�@i%@hĜ@hĜ@h�u@h�u@h1'@hb@g�@g|�@g;d@g
=@fff@f5?@e�@e��@eO�@d�/@dz�@d�D@d�@cS�@co@b�@b�H@b�!@b��@b^5@b-@a��@`Ĝ@`r�@_�@_��@_+@^�R@^v�@^V@^E�@]@]�@]/@\�@\�D@\j@[�F@[t�@Z��@Z=q@Z�@ZJ@Y�#@Y�^@Yx�@Y7L@X�9@XQ�@XQ�@XA�@W�@W��@W�P@W;d@W+@W
=@V�y@V��@Vv�@VV@U�@U��@U�@T�/@Tj@T1@S��@SC�@R�!@QG�@PĜ@P��@P�u@Pr�@P �@O�@O�w@O�P@Ol�@O
=@N�@N�+@N5?@M�h@M/@L��@L�/@L�D@K�m@KdZ@J��@J�\@JM�@Ix�@I7L@H�@HA�@G�@G�w@G|�@G�@F�R@F��@FV@F5?@F$�@F{@F@E�@E�T@E��@D��@D��@Dj@D(�@C��@C�
@C�
@Cƨ@Cƨ@C�F@C��@CC�@B�!@B�@A�#@A�^@A��@Ax�@A&�@@��@@�9@@�@@A�@@1'@@ �@?�@?��@?��@?�@?l�@?+@?
=@>�@>E�@>{@=�T@=��@=/@=V@=V@<�/@<�@<Z@<I�@<I�@<9X@<9X@<I�@<(�@;��@;�m@;�m@;�m@;�
@;��@;C�@;33@;33@;o@:�@:��@:~�@:^5@:-@9��@9x�@9%@8�`@8Ĝ@8��@81'@7�@7��@7�@7|�@7+@7
=@6��@6�y@6�R@6v�@6�+@6�+@6E�@5@4�@4�@4j@4�@3ƨ@3�@3dZ@333@3"�@3o@3o@2�@2�H@2��@2�!@2M�@2J@1�^@17L@0��@0�u@0 �@/�@/�@/�;@/�w@/�@/�P@/K�@/�@.��@.�R@.��@.�+@.V@.$�@-�@-V@,��@,�D@,z�@,I�@+�
@+C�@*��@*~�@*^5@*M�@*J@)�^@)x�@)hs@)X@)7L@(��@(�`@(�@(1'@(b@(  @'�@'�w@'�P@'\)@'+@'
=@&�y@&��@&��@&5?@%@%�h@%?}@$��@$�@$Z@$(�@$�@$1@#�
@#�F@#dZ@#o@"��@"��@"��@"�\@!�#@!x�@!X@!&�@ ��@ bN@ b@   @��@l�@
=@�y@�R@��@��@�+@ff@{@��@��@?}@��@�@j@�m@�F@�F@�F@�@S�@C�@"�@�H@�\@~�@^5@-@�@��@7L@��@r�@bN@A�@1'@  @�@�P@l�@\)@\)@\)@K�@��@�@�+@@��@O�@�/@�/@��@�@j@9X@�@�m@C�@��@�\@~�@^5@J@��@��@x�@hs@X@7L@%@��@�9@�u@Q�@b@�@�@��@�@��@�P@|�@l�@l�@+@
=@��@�@v�@V@5?@5?@{@{@{@{@{@{@@�T@�-@�h@p�@O�@��@I�@��@ƨ@�@t�@C�@
�@
^5@
J@	��@	�@	�#@	�#@	�#@	��@	��@	�^@	�^@	��@	�7@	�7@	X@	X@	7L@	�@	%@��@��@Ĝ@Ĝ@Ĝ@�9@�@Q�@1'@�@�w@�P@l�@K�@;d@��@�@�+@@@�-@�h@O�@�j@j@Z@9X@1@��@�m@�F@��@S�@o@�@��@�!@�!@��@��@�\@n�@n�@n�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B{B{B{B{B{B{B{B{B{B{B{BuBuBuBuBuBoBoBbBPB
=BB��BƨB��B��B��B�!BǮB�/B�5B��BɺBB�9B��B��B�{B��B�B��B�B>wB	7B�sBŢB��B{�Be`BN�B@�B:^B33B'�B�BDB
��B
��B
�B
�
B
��B
��B
��B
��B
ƨB
ÖB
�dB
�-B
�B
��B
��B
�VB
�%B
|�B
s�B
l�B
gmB
aHB
cTB
`BB
ZB
S�B
J�B
C�B
@�B
<jB
6FB
.B
$�B
�B
�B
{B
hB
VB
PB

=B
1B
%B
+B
B
B	��B	��B	��B	�B	�TB	�`B	�BB	��B	ȴB	�wB	�XB	�RB	�?B	�-B	�B	�B	�B	�B	��B	��B	��B	�bB	�VB	�=B	�B	y�B	x�B	r�B	iyB	bNB	`BB	bNB	`BB	[#B	R�B	H�B	E�B	F�B	B�B	5?B	-B	/B	0!B	-B	(�B	!�B	!�B	�B	JB	uB	DB	
=B	\B	VB	PB	DB	%B��B�B�B�5B�#B�NB�TB�HB�;B�;B�)B�B�B��B��B��B��BŢB��B�XB�?B�-B�RB�RB�LB�9B�B��B��B�uB�uB��B�uB�\B�oB�bB�+B�Bw�B�B�B�B�%B�%B�%B�B�B�B�B�B{�Bs�BjBjBjBm�Bl�BffB_;Be`B_;B^5B`BB\)BW
BZBT�BVB[#BZBW
BS�BO�BM�BM�BQ�BO�BL�BK�BH�BP�BP�BN�BJ�BE�B<jB9XBD�BC�BF�BG�BE�BC�B>wB8RB(�B'�B9XB9XB9XB9XB=qB<jB<jB=qB<jB9XB5?B5?B1'B&�B"�B49B5?B33B2-B5?B2-B0!B/B7LB33B7LB:^B7LB49B7LB7LB7LB6FB6FB49B5?B33B1'B(�B&�B2-B7LB8RB:^B;dB?}B@�BA�BA�BC�BE�BG�BH�BI�BG�BC�BG�BJ�BJ�BH�BL�BP�BN�BK�BL�BP�BL�BO�BP�BR�BT�BT�BW
BVBXBZB[#BZBZB\)B^5B^5B^5B`BB`BBaHBaHB_;BdZBiyBo�Bq�Br�Bp�Bt�Bz�Bz�Bz�B~�B� B� B� B�B�B�B�B�B�B�B�B�7B�JB�VB�\B�\B�\B�\B�oB��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�'B�!B�!B�3B�9B�FB�^B��BƨBɺB��B��B��B��B��B�
B�B�B�#B�HB�mB�B�B�B�B�B��B��B	B	
=B	DB	\B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	'�B	'�B	(�B	(�B	(�B	+B	)�B	(�B	.B	0!B	2-B	33B	5?B	5?B	7LB	8RB	:^B	<jB	>wB	@�B	B�B	B�B	C�B	C�B	G�B	G�B	H�B	I�B	J�B	N�B	P�B	O�B	P�B	XB	[#B	[#B	\)B	^5B	`BB	aHB	e`B	gmB	hsB	l�B	m�B	o�B	o�B	q�B	p�B	o�B	r�B	t�B	w�B	x�B	}�B	�B	�B	�B	�1B	�PB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�-B	�-B	�3B	�3B	�9B	�3B	�3B	�jB	�qB	��B	��B	��B	��B	��B	ÖB	ĜB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�/B	�)B	�/B	�;B	�HB	�NB	�HB	�NB	�TB	�ZB	�fB	�fB	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
	7B
	7B
JB
JB
DB
PB
PB
\B
\B
\B
\B
\B
\B
hB
hB
oB
uB
uB
uB
uB
uB
uB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
!�B
!�B
!�B
!�B
!�B
#�B
#�B
$�B
$�B
$�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
$�B
$�B
%�B
$�B
%�B
$�B
%�B
&�B
'�B
'�B
&�B
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
+B
)�B
)�B
-B
-B
.B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
1'B
0!B
0!B
1'B
1'B
1'B
2-B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
5?B
49B
6FB
9XB
9XB
8RB
8RB
8RB
9XB
:^B
;dB
;dB
:^B
;dB
;dB
<jB
<jB
<jB
<jB
=qB
<jB
=qB
>wB
?}B
?}B
?}B
>wB
?}B
?}B
?}B
@�B
@�B
@�B
?}B
?}B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
H�B
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
M�B
N�B
N�B
M�B
M�B
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
Q�B
Q�B
Q�B
R�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
R�B
R�B
R�B
R�B
R�B
S�B
T�B
T�B
W
B
W
B
VB
W
B
W
B
W
B
W
B
VB
W
B
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
\)B
[#B
\)B
\)B
]/B
]/B
]/B
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
_;B
_;B
`BB
`BB
`BB
_;B
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
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
iyB
iyB
iyB
jB
k�B
k�B
jB
jB
k�B
m�B
m�B
l�B
m�B
m�B
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
o�B
o�B
p�B
p�B
p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B{B�B�B{B�B�B�B�B{B�B�BuB�B�B�B�B�B�B�B�BB�B��B�=B�XB��B��B��B�EB�dB߾B�vB�=BÖB�+B�hB�]B�B��B��B��B��BD�B<B��B�#B�xB�BiyBS@BB�B<B5B)�B!�BB
��B
�RB
�B
چB
�B
�NB
�B
��B
�zB
�gB
��B
��B
�WB
��B
��B
��B
��B
~wB
utB
m�B
h�B
b�B
c�B
`�B
[=B
UgB
LdB
E9B
AUB
=qB
7fB
/�B
&�B
1B
mB
2B
 B
�B
�B

�B
�B
�B
_B
SB
�B	��B	��B	�zB	�!B	�FB	��B	�bB	өB	ʦB	��B	�B	�XB	�+B	�B	�B	��B	�]B	�kB	�sB	�B	�qB	�:B	��B	�^B	��B	{JB	zB	tB	k�B	dB	a-B	b�B	`�B	[�B	TaB	J�B	F�B	GzB	C�B	7�B	/5B	0UB	0�B	-�B	)�B	# B	"�B	�B	<B	,B	B	B	vB	�B	�B	�B	B��B�ZB��B�B��B��B�B��B��BߤBܬB��B��BуB��B�PB�dB��B��B�B�2B��B��B��B��B��B��B��B�;B��B�gB��B�B��B�B�B��B��Bz^B��B��B��B�YB�?B�YB�SB�gB�gB�uB�oB|�BuZBl�Bk�BlBn/BmCBg�B`�Be�B`�B_VBaB]dBXEBZ�BVSBV�B[�BZ�BW�BT�BQ BOBOBR�BP�BM�BL�BJ#BQNBQNBOvBK�BF�B>�B;BE�BD�BG_BHKBFYBDgB?�B:*B,�B*B:*B:DB:DB:B=�B<�B<�B=�B<�B9�B6B6+B2|B)DB%�B5B5�B4B3B5�B33B1[B0UB7�B4TB7�B:�B8B5?B8B7�B8B7B72B5?B6+B49B2|B+B)yB3MB8B8�B:�B;�B?�B@�BA�BA�BC�BE�BG�BH�BJ	BH1BD�BHKBKDBK^BI�BM�BQNBO�BL�BM�BQ�BNVBP�BRBS�BU�BU�BW�BV�BX�BZ�B[qBZ�BZ�B\�B^�B^�B^�B`�B`�Ba�Ba�B`'BeBjBo�Bq�BshBqvBu�B{JB{0B{dBHB�OB�4B�OB�UB�oB�oB�aB�[B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�YB�kB��B��B�=B�]B�cB�IB�iB�oB�vB��B��B��B��B�B�B�JB�'B�+B�#B�6B�VB�NB�FB�aB�YBؓBٚB��B��B��B��B��B��B��B�3B�tB��B	�B	
rB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	CB	;B	&LB	(>B	(>B	)*B	)*B	)*B	+6B	*eB	)�B	.}B	0oB	2�B	3�B	5tB	5�B	7�B	8�B	:�B	<�B	>�B	@�B	B�B	B�B	C�B	DB	G�B	G�B	IB	J	B	KB	OB	Q B	PB	QB	XEB	[WB	[�B	\�B	^�B	`�B	a�B	ezB	g�B	h�B	l�B	m�B	o�B	pB	q�B	qB	o�B	r�B	t�B	xB	y>B	~B	�B	�3B	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�*B	�*B	�0B	�0B	�IB	�AB	�AB	�AB	�AB	�AB	�AB	�AB	�GB	�GB	�hB	�hB	�nB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ĶB	��B	��B	��B	�B	�B	�B	�B	�B	��B	�B	�B	�4B	� B	�&B	�:B	�TB	�9B	�EB	�EB	�EB	�EB	�EB	�1B	�yB	�_B	�kB	�kB	�dB	�xB	ݘB	�pB	�|B	�B	�B	�hB	�B	�B	�B	�B	��B	�B	�B	��B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	�B	�B	�8B	�$B	�$B	�>B	�XB	�<B
 B
 4B
 4B
 4B
;B
;B
;B
;B
;B
 B
AB
AB
uB
SB
YB
_B
zB
zB
	�B
	�B
~B
�B
�B
�B
�B
�B
�B
�B
vB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
�B
�B
�B
 B
!�B
!�B
!�B
!�B
!�B
#�B
#�B
$�B
$�B
$�B
$B
$B
#�B
#�B
#�B
#�B
$B
$B
$�B
$�B
%B
%B
%B
%B
&B
%B
&B
%,B
&2B
'B
($B
($B
'8B
)*B
)*B
)*B
*0B
*0B
*0B
+B
+6B
+6B
+6B
,=B
,=B
+6B
*KB
*eB
-CB
-]B
.IB
.IB
/5B
0UB
0UB
0;B
0!B
0;B
0UB
1AB
1AB
0UB
0oB
1[B
1vB
1vB
2aB
3MB
3�B
4nB
5?B
5ZB
5tB
5tB
5tB
5tB
5tB
5tB
5ZB
6`B
6zB
6zB
6`B
5tB
4�B
6zB
9rB
9�B
8�B
8�B
8�B
9�B
:xB
;�B
;�B
:�B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
<�B
=�B
>�B
?�B
?�B
?�B
>�B
?�B
?�B
?�B
@�B
@�B
@�B
?�B
?�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
E�B
F�B
G�B
G�B
G�B
G�B
H�B
I�B
I�B
IB
I�B
J�B
J�B
K�B
K�B
K�B
LB
K�B
K�B
K�B
K�B
K�B
L�B
MB
MB
NB
N�B
N�B
NB
M�B
N�B
OB
OB
OB
O�B
PB
PB
PB
PB
P.B
P.B
R B
RB
R B
SB
R:B
R:B
S&B
S&B
TB
S�B
TB
S&B
S&B
S&B
S@B
S@B
T,B
UB
U2B
W$B
W$B
V9B
W?B
W?B
W?B
W?B
VSB
WYB
Y1B
YKB
YKB
YKB
Y1B
ZQB
ZQB
[=B
[=B
[WB
[=B
[=B
[=B
\]B
[WB
\]B
\xB
]/B
]~B
]dB
]IB
]/B
]IB
]IB
]IB
]dB
]dB
^OB
^jB
^jB
_pB
_pB
`\B
`\B
`BB
`BB
`\B
`\B
`\B
`\B
_pB
_pB
`vB
`vB
`vB
_�B
`vB
a|B
b�B
b�B
b�B
bhB
b�B
b�B
d�B
ezB
ezB
e�B
ezB
ezB
ezB
e`B
e�B
ezB
ezB
e�B
ezB
e�B
ezB
e�B
e�B
e`B
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
g�B
h�B
h�B
h�B
h�B
i�B
i�B
i�B
j�B
k�B
k�B
j�B
j�B
k�B
m�B
m�B
l�B
m�B
m�B
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
o�B
o�B
p�B
p�B
p�11111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.19(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801060032282018010600322820180106003228201806221235442018062212354420180622123544201804050432052018040504320520180405043205  JA  ARFMdecpA19c                                                                20180102063511  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180101213512  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180101213515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180101213515  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180101213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180101213516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180101213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180101213516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180101213516  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180101213516                      G�O�G�O�G�O�                JA  ARUP                                                                        20180101215452                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180102153521  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180105000000  CF  PSAL_ADJUSTED_QCB�  B�  G�O�                JM  ARCAJMQC2.0                                                                 20180105153228  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180105153228  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193205  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033544  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121517                      G�O�G�O�G�O�                