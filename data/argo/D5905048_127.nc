CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2017-06-09T00:35:20Z creation;2017-06-09T00:35:23Z conversion to V3.1;2019-12-19T08:05:23Z update;     
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
resolution        =���   axis      Z        \  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \,   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  s8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  �0   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     \  ˌ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20170609003520  20200116211517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  I2_0577_127                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @���L��1   @��F @3	7KƧ��d_�N;�61   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�3C(� C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ D�|�D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�	�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB��B'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%��C(O\C)�\C+�\C-�\C/��C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��DmqD��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D�=Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�}D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�6�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D��D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D��D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�v�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D��D�9�D�y�D��D���D�9�D�y�D��D��D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aա�Aՙ�AՕ�AՏ\AՃA�~�A�~�A�~�A�~�A�~�A�~�AՁAՁAՅAՉ7AՏ\AՓuA՗�A՛�AնFA�VA֑hA��mA�VA�Q�A�1'A��A֕�AՃA�$�A��A�x�AՕ�A���A��A�v�A�{AѶFA�=qAϡ�A��HA�\)A̓uA�&�A˝�A�O�A�{Aʰ!A�n�A�hsA���A�n�Aȟ�A�;dA���Aǥ�A��mA�~�AăAÝ�A�VA�$�A�^5A��A�"�A�M�A�\)A�A�{A�JA�ZA�oA�|�A��HA�|�A��hA�I�A�%A��A�oA��!A�;dA�ĜA���A��A�\)A�9XA�A�Q�A�A�A���A�9XA��A���A�\)A���A��A��wA��A�~�A��
A�
=A���A��A��A��A���A�&�A��#A��A�v�A�7LA��TA��PA�
=A�K�A�$�A~ffA}�A{�Az�Ay��Aw�Au�#Aux�AuC�As��Ar��ArjAq��Al�Ai�Ahv�Af�uAd�yAbv�A`�A_�mA^bA\jAZA�AWp�AR�AO33AL�\AK7LAH�uAFz�AE;dAD^5AC`BABE�A@ȴA?�A>��A;33A9K�A8�A8 �A7S�A57LA2bNA/G�A+�;A(�\A&{A#�A!?}A �jAAJA�-AS�AA��A{A��A\)A�jA�AAoA�A�mA��A�/AM�A|�A�HA�RA��A��A�`A�A�/A��A�DA��A�PA�AbNA��AA��A
Q�A��A�A��A�PA+AM�A33A$�A�FA ��A -@�C�@�n�@�@���@�`B@��D@���@���@�33@���@��@�33@�~�@�-@�@�{@���@�G�@�j@�ff@�@�n�@�@�O�@�j@�1'@��m@�o@�@� �@��@�n�@�X@��@���@�9X@�+@�1'@�"�@�v�@�J@�-@ᙚ@�Z@߅@�V@�@�
=@�V@�9X@�@�@֗�@���@��@��@�@�7L@Ь@�  @�@��#@ͩ�@�z�@�r�@�z�@�(�@���@�|�@�K�@�o@ʸR@�^5@���@ɺ^@�G�@��`@ȓu@� �@��;@Ǿw@��@Ə\@��@���@�ƨ@öF@î@�\)@�
=@+@�M�@�^5@�=q@��@�V@��D@�  @�+@���@��7@�Ĝ@�/@� �@� �@���@��R@�bN@���@��D@�b@���@���@��!@�n�@���@��#@��7@��;@��#@���@�I�@�9X@�9X@�=q@���@�x�@���@�E�@�
=@�l�@���@���@�-@���@���@�K�@���@�=q@�@���@��7@�`B@�`B@�X@��@���@��u@�j@�bN@�A�@���@���@��@���@��@�;d@�o@��@���@���@��!@���@�^5@��@���@��-@��h@��@�p�@�?}@��/@���@�A�@��
@���@�S�@�@��@���@�@�
=@�o@�o@�o@�
=@��H@�~�@�ff@�ff@�E�@�@�x�@�O�@�/@���@�Q�@�A�@�b@��w@���@�t�@�\)@�;d@�@���@�^5@�M�@�-@��@��@���@�p�@�G�@���@�j@�(�@�1@���@��
@���@���@�|�@�S�@�33@��!@�=q@�-@��^@�x�@�`B@�G�@�7L@�Ĝ@�z�@�9X@���@��@��w@�t�@�"�@�
=@��y@�~�@�V@�5?@�{@�J@�@���@��#@���@���@�hs@�V@���@��`@���@�I�@�1@��
@���@�S�@���@�=q@��T@��7@���@��@���@�j@�b@���@��@�l�@�+@��H@���@�~�@�^5@�=q@��@���@��@�@��7@�O�@�&�@���@��j@��D@�r�@�b@��m@��P@�dZ@�\)@�@��+@�n�@�V@�-@�J@��7@��D@�9X@� �@�b@�1@�  @���@�33@���@�n�@�^5@�$�@��@��@��T@��h@�X@�G�@�/@�%@��j@�j@�I�@� �@�  @|�@K�@�@~��@~{@}��@}�-@}�@}?}@|�/@|9X@{C�@{@z��@z��@z�!@y�#@yhs@y7L@y�@x�`@x��@x�9@x�u@xr�@x  @w;d@v�R@vE�@u�h@t�/@tj@sƨ@sS�@so@r�@r�H@r=q@q�^@q��@p��@p�u@o�;@oK�@o
=@n�y@nȴ@n��@nV@n@m�@m`B@l�/@lZ@k�
@k�@k"�@j��@j~�@i�^@i��@i7L@h�`@hQ�@g�@g|�@f��@f��@e�-@e�@d�j@dZ@c�
@c�@cC�@co@b��@b^5@b=q@a�@aG�@a7L@a�@`�u@_�@_|�@_;d@^ff@]�T@]��@]`B@\��@\�@\1@[�F@[S�@Z�H@Z�\@Z~�@ZM�@Y�#@Y�^@Yx�@Y7L@X��@XQ�@W�;@W�w@W�P@W+@V��@VV@U�@U�h@UO�@T��@Tz�@T9X@T(�@T�@S�F@S"�@R�@R�H@R��@R~�@R�@Q�^@P��@P�@O�@O�P@O\)@OK�@O
=@N�+@N@M�h@Mp�@M/@MV@LI�@K�
@K�@KC�@J�@J��@J^5@J-@I��@IG�@H��@H��@H��@HĜ@H�u@H�u@H1'@G�@Gl�@G+@F��@F�y@Fȴ@F5?@E�-@D�/@Dj@D�@C33@C@B�H@B�!@B=q@A��@AX@@��@@�@@A�@?�@?��@>�y@>�R@>v�@>$�@=�@=�h@=V@<z�@;ƨ@;t�@;t�@;dZ@;C�@;o@:��@:�\@:�@9�^@9x�@9%@8�u@8r�@8bN@81'@7�;@7��@7�w@7\)@7
=@6�R@6{@5O�@5V@4z�@4�@3��@3S�@333@3"�@2��@2^5@1�#@1��@1�7@0��@0�u@0A�@0b@/�w@/K�@.�y@.ȴ@.�+@.5?@-O�@,(�@+�
@+�
@+ƨ@+��@+dZ@+o@*��@*^5@)�@)�#@)x�@)G�@)7L@(�`@(1'@'�@'|�@'
=@&��@&V@%@%O�@%V@$Z@$9X@$9X@$(�@$(�@$�@$1@#��@#��@#��@#�m@#�m@#�
@#��@#t�@#t�@#S�@#33@"�H@"�!@"�\@"�\@"n�@"J@!�#@!��@!��@!x�@ ��@ �9@ �u@  �@��@�w@�@��@�P@�P@|�@\)@K�@��@ff@{@�@@�@O�@/@��@��@�@z�@I�@(�@ƨ@��@t�@C�@"�@@�H@��@��@�\@~�@~�@^5@=q@=q@�@�@�^@�7@x�@x�@hs@X@7L@%@�u@r�@A�@�@�P@|�@�@�y@�R@��@�+@E�@$�@�@�T@��@O�@��@��@��@��@�D@Z@(�@��@�m@�m@�m@�
@ƨ@�F@��@S�@�@�H@��@��@��@��@��@�!@��@=q@J@J@J@�@�#@�^@��@hs@�@��@�`@�`@��@��@��@ �@  @��@��@�P@�P@�P@\)@+@�@�R@�R@��@�+@v�@v�@ff@V@V@E�@�T@��@�h@�@�@O�@�j@z�@j@Z@(�@�m@ƨ@��@t�@C�@"�@o@
�H@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aա�Aՙ�AՕ�AՏ\AՃA�~�A�~�A�~�A�~�A�~�A�~�AՁAՁAՅAՉ7AՏ\AՓuA՗�A՛�AնFA�VA֑hA��mA�VA�Q�A�1'A��A֕�AՃA�$�A��A�x�AՕ�A���A��A�v�A�{AѶFA�=qAϡ�A��HA�\)A̓uA�&�A˝�A�O�A�{Aʰ!A�n�A�hsA���A�n�Aȟ�A�;dA���Aǥ�A��mA�~�AăAÝ�A�VA�$�A�^5A��A�"�A�M�A�\)A�A�{A�JA�ZA�oA�|�A��HA�|�A��hA�I�A�%A��A�oA��!A�;dA�ĜA���A��A�\)A�9XA�A�Q�A�A�A���A�9XA��A���A�\)A���A��A��wA��A�~�A��
A�
=A���A��A��A��A���A�&�A��#A��A�v�A�7LA��TA��PA�
=A�K�A�$�A~ffA}�A{�Az�Ay��Aw�Au�#Aux�AuC�As��Ar��ArjAq��Al�Ai�Ahv�Af�uAd�yAbv�A`�A_�mA^bA\jAZA�AWp�AR�AO33AL�\AK7LAH�uAFz�AE;dAD^5AC`BABE�A@ȴA?�A>��A;33A9K�A8�A8 �A7S�A57LA2bNA/G�A+�;A(�\A&{A#�A!?}A �jAAJA�-AS�AA��A{A��A\)A�jA�AAoA�A�mA��A�/AM�A|�A�HA�RA��A��A�`A�A�/A��A�DA��A�PA�AbNA��AA��A
Q�A��A�A��A�PA+AM�A33A$�A�FA ��A -@�C�@�n�@�@���@�`B@��D@���@���@�33@���@��@�33@�~�@�-@�@�{@���@�G�@�j@�ff@�@�n�@�@�O�@�j@�1'@��m@�o@�@� �@��@�n�@�X@��@���@�9X@�+@�1'@�"�@�v�@�J@�-@ᙚ@�Z@߅@�V@�@�
=@�V@�9X@�@�@֗�@���@��@��@�@�7L@Ь@�  @�@��#@ͩ�@�z�@�r�@�z�@�(�@���@�|�@�K�@�o@ʸR@�^5@���@ɺ^@�G�@��`@ȓu@� �@��;@Ǿw@��@Ə\@��@���@�ƨ@öF@î@�\)@�
=@+@�M�@�^5@�=q@��@�V@��D@�  @�+@���@��7@�Ĝ@�/@� �@� �@���@��R@�bN@���@��D@�b@���@���@��!@�n�@���@��#@��7@��;@��#@���@�I�@�9X@�9X@�=q@���@�x�@���@�E�@�
=@�l�@���@���@�-@���@���@�K�@���@�=q@�@���@��7@�`B@�`B@�X@��@���@��u@�j@�bN@�A�@���@���@��@���@��@�;d@�o@��@���@���@��!@���@�^5@��@���@��-@��h@��@�p�@�?}@��/@���@�A�@��
@���@�S�@�@��@���@�@�
=@�o@�o@�o@�
=@��H@�~�@�ff@�ff@�E�@�@�x�@�O�@�/@���@�Q�@�A�@�b@��w@���@�t�@�\)@�;d@�@���@�^5@�M�@�-@��@��@���@�p�@�G�@���@�j@�(�@�1@���@��
@���@���@�|�@�S�@�33@��!@�=q@�-@��^@�x�@�`B@�G�@�7L@�Ĝ@�z�@�9X@���@��@��w@�t�@�"�@�
=@��y@�~�@�V@�5?@�{@�J@�@���@��#@���@���@�hs@�V@���@��`@���@�I�@�1@��
@���@�S�@���@�=q@��T@��7@���@��@���@�j@�b@���@��@�l�@�+@��H@���@�~�@�^5@�=q@��@���@��@�@��7@�O�@�&�@���@��j@��D@�r�@�b@��m@��P@�dZ@�\)@�@��+@�n�@�V@�-@�J@��7@��D@�9X@� �@�b@�1@�  @���@�33@���@�n�@�^5@�$�@��@��@��T@��h@�X@�G�@�/@�%@��j@�j@�I�@� �@�  @|�@K�@�@~��@~{@}��@}�-@}�@}?}@|�/@|9X@{C�@{@z��@z��@z�!@y�#@yhs@y7L@y�@x�`@x��@x�9@x�u@xr�@x  @w;d@v�R@vE�@u�h@t�/@tj@sƨ@sS�@so@r�@r�H@r=q@q�^@q��@p��@p�u@o�;@oK�@o
=@n�y@nȴ@n��@nV@n@m�@m`B@l�/@lZ@k�
@k�@k"�@j��@j~�@i�^@i��@i7L@h�`@hQ�@g�@g|�@f��@f��@e�-@e�@d�j@dZ@c�
@c�@cC�@co@b��@b^5@b=q@a�@aG�@a7L@a�@`�u@_�@_|�@_;d@^ff@]�T@]��@]`B@\��@\�@\1@[�F@[S�@Z�H@Z�\@Z~�@ZM�@Y�#@Y�^@Yx�@Y7L@X��@XQ�@W�;@W�w@W�P@W+@V��@VV@U�@U�h@UO�@T��@Tz�@T9X@T(�@T�@S�F@S"�@R�@R�H@R��@R~�@R�@Q�^@P��@P�@O�@O�P@O\)@OK�@O
=@N�+@N@M�h@Mp�@M/@MV@LI�@K�
@K�@KC�@J�@J��@J^5@J-@I��@IG�@H��@H��@H��@HĜ@H�u@H�u@H1'@G�@Gl�@G+@F��@F�y@Fȴ@F5?@E�-@D�/@Dj@D�@C33@C@B�H@B�!@B=q@A��@AX@@��@@�@@A�@?�@?��@>�y@>�R@>v�@>$�@=�@=�h@=V@<z�@;ƨ@;t�@;t�@;dZ@;C�@;o@:��@:�\@:�@9�^@9x�@9%@8�u@8r�@8bN@81'@7�;@7��@7�w@7\)@7
=@6�R@6{@5O�@5V@4z�@4�@3��@3S�@333@3"�@2��@2^5@1�#@1��@1�7@0��@0�u@0A�@0b@/�w@/K�@.�y@.ȴ@.�+@.5?@-O�@,(�@+�
@+�
@+ƨ@+��@+dZ@+o@*��@*^5@)�@)�#@)x�@)G�@)7L@(�`@(1'@'�@'|�@'
=@&��@&V@%@%O�@%V@$Z@$9X@$9X@$(�@$(�@$�@$1@#��@#��@#��@#�m@#�m@#�
@#��@#t�@#t�@#S�@#33@"�H@"�!@"�\@"�\@"n�@"J@!�#@!��@!��@!x�@ ��@ �9@ �u@  �@��@�w@�@��@�P@�P@|�@\)@K�@��@ff@{@�@@�@O�@/@��@��@�@z�@I�@(�@ƨ@��@t�@C�@"�@@�H@��@��@�\@~�@~�@^5@=q@=q@�@�@�^@�7@x�@x�@hs@X@7L@%@�u@r�@A�@�@�P@|�@�@�y@�R@��@�+@E�@$�@�@�T@��@O�@��@��@��@��@�D@Z@(�@��@�m@�m@�m@�
@ƨ@�F@��@S�@�@�H@��@��@��@��@��@�!@��@=q@J@J@J@�@�#@�^@��@hs@�@��@�`@�`@��@��@��@ �@  @��@��@�P@�P@�P@\)@+@�@�R@�R@��@�+@v�@v�@ff@V@V@E�@�T@��@�h@�@�@O�@�j@z�@j@Z@(�@�m@ƨ@��@t�@C�@"�@o@
�H@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	I�B	H�B	I�B	H�B	I�B	I�B	I�B	I�B	I�B	I�B	J�B	J�B	J�B	K�B	M�B	N�B	O�B	R�B	VB	`BB	�+B	�;B
/B
iyB
�B
�%B
�1B
�+B
l�B
]/B
]/B
�%B
�3B
�B
��B
�uB
��B1B
�B%B
�B
�`B
��B
��B
�B
��B%B�B0!Bz�B�LB�dB�B�9B��B�#B�BDB7LBB�BbNBaHBffBiyBq�By�B�B�1B�JB�uB�hBy�BR�BL�BI�BC�B>wB?}B>wB:^B6FB5?B2-B-B'�B$�B�B�B-B=qB!�B��B�ZB��BȴB�FB}�Bz�Bt�Bo�Bl�BbNBP�BK�BC�B9XB2-B$�B�BB
��B
�NB
�)B
�
B
��B
�B
��B
�\B
�B
|�B
t�B
k�B
_;B
T�B
O�B
N�B
D�B
>wB
8RB
0!B
uB	��B	�B	�HB	�B	ǮB	�B	�B	��B	��B	�+B	u�B	YB	=qB	'�B	�B	bB	B	B��B��B�B�B�mB�NB�/B��B��B��BȴB��B�-B��B��B�\B�oB�PB�B�DB�\B�VB�bB�VB�bB�\B�\B�VB�PB�VB�PB�=B�7B�=B��B��B��B��B��B�B�3B�^B�wBÖBŢBƨB��B��B��B��B��B��B��B��B��B�9B��B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�9B�jBB��B��B��B��B��B�B�B�5B�5B�BB�HB�TB�`B�`B�`B�B�B�B�B��B��B��B��B��B��B��B�B��B��B��B	  B��B��B��B��B��B��B��B��B��B��B	B	B	B	%B	%B	JB	bB	oB	oB	{B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	$�B	$�B	#�B	"�B	%�B	+B	.B	/B	1'B	2-B	1'B	33B	6FB	B�B	C�B	B�B	C�B	C�B	;dB	=qB	E�B	G�B	D�B	D�B	C�B	D�B	T�B	W
B	YB	YB	ZB	YB	XB	W
B	XB	YB	XB	ZB	YB	XB	[#B	^5B	YB	W
B	ZB	_;B	ffB	jB	p�B	q�B	q�B	q�B	p�B	n�B	q�B	s�B	s�B	u�B	u�B	w�B	x�B	x�B	x�B	z�B	}�B	� B	�B	�B	�B	�%B	�1B	�7B	�7B	�7B	�JB	�VB	�bB	�bB	�bB	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�9B	�?B	�FB	�LB	�RB	�XB	�XB	�^B	�dB	�jB	�wB	�wB	�wB	��B	��B	��B	ÖB	ŢB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�;B	�BB	�BB	�TB	�TB	�ZB	�ZB	�ZB	�fB	�fB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
%B
1B
	7B
	7B

=B

=B
DB
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
bB
hB
hB
hB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
#�B
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
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
+B
+B
,B
,B
,B
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
0!B
1'B
1'B
2-B
2-B
2-B
33B
33B
33B
49B
33B
49B
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
8RB
9XB
9XB
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
;dB
;dB
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
>wB
>wB
>wB
?}B
?}B
?}B
?}B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
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
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
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
N�B
N�B
N�B
N�B
N�B
N�B
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
Q�B
R�B
R�B
S�B
S�B
S�B
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
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
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
cTB
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
e`B
e`B
e`B
e`B
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
gmB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
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
k�B
k�B
l�B
k�B
l�B
l�B
l�B
l�B
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
r�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
r�B
r�B
s�B
s�B
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
t�B
t�B
u�B
u�B
u�B
u�B
v�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
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
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	I�B	H�B	I�B	H�B	I�B	I�B	I�B	I�B	I�B	I�B	J�B	J�B	J�B	K�B	M�B	N�B	O�B	R�B	U�B	_�B	��B	ޞB
.cB
iDB
�mB
��B
�RB
��B
mCB
]IB
\�B
��B
�TB
�cB
�+B
�B
��B
=B
�B�B
�"B
�B
�&B
�B
�CB
�dBB$B0�B|B��B��B� B�%B��B�/B�]B�B9$BEBcBcBg�BkBs�B{�B��B�=B�\B��B��B~BBT�BN<BLBF�BAUBB'B?�B;�B8B7�B4�B/5B+B*�B�BB3�BB�B'B�"B�B�,B̈́B��B�iB|�Bv+BqvBn�Bd�BSBNpBE�B;dB5�B(
BBB
�RB
�nB
ݘB
ٴB
�MB
��B
��B
�B
��B
~�B
v�B
n/B
`�B
U�B
P�B
P�B
E�B
?�B
:�B
5�B
�B	��B	�B	�B	��B	�	B	�iB	�wB	�TB	��B	�^B	z�B	]~B	@�B	*KB	�B	�B	�B	aB�cB��B��B�iB�yB�fB�VB� B͹B�~B��BŢB��B�yB��B��B��B��B��B�PB��B�B� B�B� B�.B�B�B��B��B��B�^B�lB��B�QB��B��B��B��B�CB�hB�^B��B��B�?B�1B̈́B̳BΊB��B̳B��BуB�gB� B�fB��B� B�B��B�FB�TB��B��B��B��B�sB�9B��B��B�B�KB�+B��B�KB��B��B�#B�IB�!B��B��B�B��B��B�TB�nB��B�-B�B�JB�dB�hB��B�
B�QB��B�!B��B�B��B��B�B�RB�cB�AB��B�'B�?B��B��B��B��B��B�B�B��B�lB�B	;B��B��B��B��B�`B�tB��B��B�JB��B	;B	GB	�B	�B	�B	~B	�B	�B	�B	�B	�B	B	�B	!B	 'B	!B	"4B	#�B	$ZB	%�B	%�B	$�B	# B	&B	+kB	.}B	/�B	1vB	2aB	1vB	3�B	7B	B�B	DMB	C�B	ESB	E�B	<B	=�B	FYB	G�B	EB	E�B	D�B	D�B	UB	W�B	Y�B	Y�B	ZkB	YeB	XyB	WsB	X�B	ZkB	YeB	Z�B	Y�B	XEB	[�B	_VB	Y�B	W?B	ZB	^�B	fB	j�B	q'B	rB	raB	r�B	q[B	oB	r-B	tB	tB	u�B	u�B	xB	y$B	y	B	y$B	{0B	~BB	�4B	�'B	�[B	�aB	�tB	�fB	�lB	�lB	�lB	��B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�@B	�RB	�>B	�DB	�KB	�"B	�)B	�5B	�AB	�MB	�TB	�ZB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	żB	��B	��B	��B	�	B	�B	�B	�B	��B	�B	�B	�.B	�NB	�@B	�aB	ևB	�_B	�KB	�KB	�1B	�QB	�QB	�WB	�qB	�xB	ܬB	ߤB	�vB	�B	�B	�B	�tB	�tB	��B	�B	�B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�8B	�^B	�dB	�PB	�VB	�cB
 4B
 OB
{B
tB
�B
	lB
	�B

�B

�B
xB
~B
~B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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

B
EB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
!B
 'B
�B
�B
 �B
 �B
!-B
"B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"4B
# B
#:B
#:B
$@B
$&B
$&B
%,B
%B
&B
&B
&2B
&2B
&B
'8B
'8B
'RB
(>B
($B
)*B
)*B
)*B
)*B
)*B
)*B
)DB
*KB
+QB
+QB
,=B
,WB
,WB
-CB
-]B
-CB
.IB
.cB
.cB
.cB
/iB
/iB
/iB
/�B
0UB
1vB
1[B
2|B
2aB
2|B
3hB
3�B
3hB
4nB
3�B
4nB
4nB
4nB
4�B
5tB
5�B
5�B
5�B
6�B
7�B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
:xB
;�B
;�B
;�B
;�B
;�B
;�B
;�B
;�B
;�B
;�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?�B
?�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
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
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
F�B
F�B
G�B
HB
IB
IB
IB
I�B
I�B
J	B
J#B
KB
KB
J�B
K�B
K�B
K�B
LB
MB
MB
MB
MB
MB
MB
N"B
N"B
N"B
OB
N�B
OB
OB
OB
OB
O�B
P.B
P.B
QB
Q4B
QB
R B
R B
R B
R B
R B
R B
R:B
R:B
R:B
S@B
S@B
T,B
TFB
T,B
U2B
U2B
V9B
V9B
V9B
VSB
VSB
W$B
W$B
WYB
XEB
XEB
XEB
Y1B
YeB
YKB
YKB
YKB
ZkB
Z�B
[�B
\]B
\CB
\]B
\]B
\]B
\]B
]dB
]IB
]~B
^jB
^OB
^jB
^jB
^�B
_pB
_VB
_�B
`�B
`�B
`�B
a�B
a�B
bhB
b�B
cnB
cnB
cnB
cTB
cnB
cnB
cnB
cnB
cTB
cnB
cnB
cnB
c�B
c�B
cnB
c�B
c�B
d�B
d�B
d�B
dtB
d�B
d�B
e�B
ezB
e�B
e�B
e�B
f�B
f�B
f�B
f�B
g�B
g�B
gmB
g�B
gmB
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
i�B
i�B
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
l�B
k�B
l�B
l�B
l�B
l�B
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
r�B
r�B
r�B
r�B
r�B
s�B
r�B
s�B
s�B
r�B
r�B
s�B
s�B
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
t�B
t�B
u�B
u�B
u�B
u�B
v�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
y	B
x�B
x�B
x�B
y	B
y$B
zB
y�B
zB
zB
zB
{B
{B
z�B
{B
{B
z�B
{B
{�B
|11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.19(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201706130037192017061300371920170613003719201806221314372018062213143720180622131437201804050716172018040507161720180405071617  JA  ARFMdecpA19c                                                                20170609093510  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20170609003520  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20170609003522  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20170609003522  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20170609003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20170609003523  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20170609003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20170609003523  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20170609003523  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20170609003523                      G�O�G�O�G�O�                JA  ARUP                                                                        20170609010841                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20170609153424  CV  JULD            G�O�G�O�F�m�                JM  ARCAJMQC2.0                                                                 20170612153719  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20170612153719  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404221617  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622041437  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116211517                      G�O�G�O�G�O�                