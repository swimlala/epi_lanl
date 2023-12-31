CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-05-14T00:35:58Z creation;2018-05-14T00:36:04Z conversion to V3.1;2019-12-19T07:42:25Z update;     
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \|   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �|   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �p   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �x   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �|   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180514003558  20200115131518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_239                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�bu�RL�1   @�bvq��@:?�䎊r�dWdZ�1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�3D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�<�DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�C3Dـ D�� D���D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�I�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@s�
@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_��Bg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY��C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��DemqDe��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D��D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D��D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�6�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�=D�y�Dٹ�D���D�6�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D��D�C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��yA��mA��mA��TA��HA��;A��;A��/A��/A��;A��HA��HA��
A���A�ȴA��^A�;dA��uA��+A� �A���A���A���A��A��yA��
A�ƨA�ƨA���A�ƨA���A���A�I�A���A���A�v�A�A�A�
=A���A��
A��/A��uA��wA���A���A�M�A�n�A�v�A�Q�A��RA�|�A�"�A�ffA�v�A���A���A�t�A�A�A���A���A�33A�A�  A�~�A��A��+A��A�  A���A�7LA�x�A�+A�n�A��A��TA�7LA���A��A��A�bA��`A��+A~�A~9XA}��A|�yA|��A{��Az�Aw�mAu��AsAq��Ap�Ao��Ao
=An�AnjAn(�AmhsAl��AlffAk�;Ak�AkVAk�AjȴAi��Ag��Ae�wAc�Ab=qAat�A`1'A`bA_��A_��A_l�A^v�A\n�A[��A[l�A[`BAZ�AZQ�AZ1AYS�AWx�AV9XAU�
AT��AR�9AQ��AP�AO7LAN�AN��AL^5AJ�yAI��AI��AH�yAHjAH9XAG��AG�-AG\)AF�AD��AD^5AC��ABbNAAXA@�A?��A>��A=�A<�9A;�wA:��A9�wA8ĜA8A�A7��A6ĜA5�wA4��A4(�A3x�A2ĜA2ffA2E�A1��A1K�A1oA0^5A/��A/K�A/VA.��A.Q�A.JA-A,1'A+S�A+%A*�uA)��A(1'A&bNA%��A%`BA$��A$$�A#��A#G�A"M�A!�#A!��A!�A!XA!
=A�-A��A^5AA�A�A��A�
AdZA�A1A�AVA��A^5A�A��A��A�A��A��AS�AM�A�/A  A�A	�A	oA�DAJA��A��A1'A�7A33A �AE�A �j@��@�t�@�
=@��@���@��+@�V@�J@�@�p�@��@��j@��@���@��T@��@���@�S�@�@�{@�@��
@�~�@���@���@�@�&�@���@��@�@�dZ@�^@��@�ff@�hs@�Q�@ߕ�@�Q�@�n�@�7L@ם�@�"�@�E�@թ�@�p�@��;@��@���@�5?@�bN@�33@���@��@�&�@ȣ�@�S�@�@ř�@ă@�K�@�n�@�/@��@�
=@�M�@��h@���@�\)@��y@��@��/@��P@��@�ȴ@�~�@��h@���@���@�dZ@���@�&�@��D@��F@���@��^@��@��@�t�@��y@�@�O�@���@��
@���@�+@��R@�E�@��#@���@� �@��
@���@�@��\@��@��h@�?}@�&�@�V@���@��/@�z�@��m@��@��@��@�~�@���@�G�@���@�1'@���@�"�@�^5@���@�O�@��D@�1@�S�@�;d@��@��@���@���@���@�?}@��@��`@�Q�@��
@�33@�^5@�-@�$�@���@�p�@�O�@���@�z�@�I�@�(�@�b@�dZ@�"�@�o@���@�=q@�5?@��@��@���@��-@���@��j@��9@���@�bN@��
@���@�t�@�"�@�5?@�hs@�Ĝ@�A�@�1@�l�@�K�@�;d@�+@��@���@��H@�ȴ@�5?@�@�G�@��@�j@�1'@�1@��
@�dZ@�+@��y@�V@��@�x�@�/@��`@�Ĝ@��@���@��D@�r�@�Q�@�9X@�@K�@~��@~�y@~��@~v�@}�@|��@|�@{�F@{��@{��@{33@z��@z��@z^5@zJ@y�^@y��@yX@x�9@xA�@x �@x  @xb@x �@x1'@x1'@xA�@x1'@x1'@x1'@x1'@x  @w�P@w\)@w�@vȴ@vv�@v{@u�T@u�-@u��@u�@up�@u`B@u�@t�/@t�D@t�@s�
@sdZ@s"�@so@r��@rM�@qx�@p�`@p�9@p�9@p��@p�@pb@o;d@n{@mp�@l�D@k�m@k�F@k�@kC�@ko@j�@j��@j��@j�!@j�\@j=q@jJ@i�#@ihs@h�u@h �@g�@g�P@g+@g+@g�@f��@f@e@e��@e`B@d��@d�@c@b��@b��@b��@b��@b��@bn�@b-@a�#@a�7@aX@a7L@a%@`��@`��@`�u@`  @_��@_;d@_�@_
=@_
=@^��@^�@^5?@]��@]�h@]/@\�/@\z�@[ƨ@[�F@[S�@[@Z�@Z��@Z�\@Zn�@Y�@Yx�@Y&�@X��@Xr�@XA�@W�;@W
=@V�R@VE�@V{@V@U@Up�@T��@T�@TZ@T1@Sƨ@SS�@S@R��@R�!@R�\@R�@Q�#@P�`@P  @O��@O�@N��@N�+@Nff@N$�@M�@M�@M`B@M�@L�@L��@L�@L�@K�
@K��@K33@J�H@J��@J^5@I��@IG�@I&�@H��@G�@G�P@G;d@F��@F�+@FE�@F$�@F{@F@E�T@E�T@E@E�@E/@D�/@D�@DZ@D�@C��@C��@C33@Co@B�@B��@B^5@BM�@B=q@B�@A��@A�#@A�#@A��@A�^@A��@A��@Ax�@AG�@A&�@A%@@��@@�@@Q�@@ �@?�@?l�@?�@>�+@>ff@>V@>V@>V@>V@>E�@>{@=�@=��@=��@=�@=?}@<�@<�@<Z@;�F@;�@;dZ@;33@:�!@:J@9X@9&�@8��@8Ĝ@8�@8A�@8b@8  @7�;@7|�@7;d@7
=@6ȴ@6��@6{@5@5�-@5�h@5�@5?}@4�@4(�@3dZ@3@2��@2�!@2�!@2�!@2�!@2��@2��@2�\@2~�@2n�@1��@1�#@1�7@1hs@1hs@1X@0��@0Q�@0A�@01'@0  @/�;@/��@.��@.��@.v�@.ff@.ff@.V@.E�@-�@-@-�h@-?}@,�j@,�@,��@,�D@,z�@,Z@,�@+ƨ@+��@+C�@*�@*�@*�!@*^5@*-@)�#@)��@)G�@)&�@)%@(�`@(�@(b@'�;@'��@'\)@'K�@'�@&��@&ȴ@&�R@&v�@&$�@&@%@%�@%`B@%?}@%�@%V@$��@$�j@#�F@#�@#C�@"�H@"~�@"�@!��@!hs@!G�@!&�@ ��@ �u@ bN@ Q�@ Q�@ b@|�@�@5?@@�@p�@��@�D@j@I�@(�@1@�
@�
@�
@ƨ@S�@�@��@��@��@M�@�#@��@�7@G�@&�@�`@�9@�u@�@�@bN@�@�w@��@�P@|�@;d@��@�y@��@ff@5?@��@O�@V@��@��@Z@(�@(�@�@�m@��@t�@S�@�@��@��@�!@^5@^5@^5@=q@��@�#@��@�^@��@hs@G�@7L@7L@7L@&�@&�@&�@�@��@bN@A�@1'@1'@1'@1'@ �@b@�@+@
=@�+@E�@�T@��@�/@��@��@z�@Z@9X@(�@�@��@ƨ@��@��@�@t�@S�@"�@
�@
��@
M�@
J@	��@	�^@	��@	hs@	7L@	�@	%@	%@�`@��@Ĝ@�9@��@�@bN@Q�@1'@1'@b@�;@�@�P@K�@
=@�y@ȴ@��@�+@v�@E�@$�@@@p�@O�@�@�@��@�D@I�@�@1@�m@�F@��@�@�@�@�@�@�@dZ@o@o@�@�!@�\@�\@�\11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��yA��mA��mA��TA��HA��;A��;A��/A��/A��;A��HA��HA��
A���A�ȴA��^A�;dA��uA��+A� �A���A���A���A��A��yA��
A�ƨA�ƨA���A�ƨA���A���A�I�A���A���A�v�A�A�A�
=A���A��
A��/A��uA��wA���A���A�M�A�n�A�v�A�Q�A��RA�|�A�"�A�ffA�v�A���A���A�t�A�A�A���A���A�33A�A�  A�~�A��A��+A��A�  A���A�7LA�x�A�+A�n�A��A��TA�7LA���A��A��A�bA��`A��+A~�A~9XA}��A|�yA|��A{��Az�Aw�mAu��AsAq��Ap�Ao��Ao
=An�AnjAn(�AmhsAl��AlffAk�;Ak�AkVAk�AjȴAi��Ag��Ae�wAc�Ab=qAat�A`1'A`bA_��A_��A_l�A^v�A\n�A[��A[l�A[`BAZ�AZQ�AZ1AYS�AWx�AV9XAU�
AT��AR�9AQ��AP�AO7LAN�AN��AL^5AJ�yAI��AI��AH�yAHjAH9XAG��AG�-AG\)AF�AD��AD^5AC��ABbNAAXA@�A?��A>��A=�A<�9A;�wA:��A9�wA8ĜA8A�A7��A6ĜA5�wA4��A4(�A3x�A2ĜA2ffA2E�A1��A1K�A1oA0^5A/��A/K�A/VA.��A.Q�A.JA-A,1'A+S�A+%A*�uA)��A(1'A&bNA%��A%`BA$��A$$�A#��A#G�A"M�A!�#A!��A!�A!XA!
=A�-A��A^5AA�A�A��A�
AdZA�A1A�AVA��A^5A�A��A��A�A��A��AS�AM�A�/A  A�A	�A	oA�DAJA��A��A1'A�7A33A �AE�A �j@��@�t�@�
=@��@���@��+@�V@�J@�@�p�@��@��j@��@���@��T@��@���@�S�@�@�{@�@��
@�~�@���@���@�@�&�@���@��@�@�dZ@�^@��@�ff@�hs@�Q�@ߕ�@�Q�@�n�@�7L@ם�@�"�@�E�@թ�@�p�@��;@��@���@�5?@�bN@�33@���@��@�&�@ȣ�@�S�@�@ř�@ă@�K�@�n�@�/@��@�
=@�M�@��h@���@�\)@��y@��@��/@��P@��@�ȴ@�~�@��h@���@���@�dZ@���@�&�@��D@��F@���@��^@��@��@�t�@��y@�@�O�@���@��
@���@�+@��R@�E�@��#@���@� �@��
@���@�@��\@��@��h@�?}@�&�@�V@���@��/@�z�@��m@��@��@��@�~�@���@�G�@���@�1'@���@�"�@�^5@���@�O�@��D@�1@�S�@�;d@��@��@���@���@���@�?}@��@��`@�Q�@��
@�33@�^5@�-@�$�@���@�p�@�O�@���@�z�@�I�@�(�@�b@�dZ@�"�@�o@���@�=q@�5?@��@��@���@��-@���@��j@��9@���@�bN@��
@���@�t�@�"�@�5?@�hs@�Ĝ@�A�@�1@�l�@�K�@�;d@�+@��@���@��H@�ȴ@�5?@�@�G�@��@�j@�1'@�1@��
@�dZ@�+@��y@�V@��@�x�@�/@��`@�Ĝ@��@���@��D@�r�@�Q�@�9X@�@K�@~��@~�y@~��@~v�@}�@|��@|�@{�F@{��@{��@{33@z��@z��@z^5@zJ@y�^@y��@yX@x�9@xA�@x �@x  @xb@x �@x1'@x1'@xA�@x1'@x1'@x1'@x1'@x  @w�P@w\)@w�@vȴ@vv�@v{@u�T@u�-@u��@u�@up�@u`B@u�@t�/@t�D@t�@s�
@sdZ@s"�@so@r��@rM�@qx�@p�`@p�9@p�9@p��@p�@pb@o;d@n{@mp�@l�D@k�m@k�F@k�@kC�@ko@j�@j��@j��@j�!@j�\@j=q@jJ@i�#@ihs@h�u@h �@g�@g�P@g+@g+@g�@f��@f@e@e��@e`B@d��@d�@c@b��@b��@b��@b��@b��@bn�@b-@a�#@a�7@aX@a7L@a%@`��@`��@`�u@`  @_��@_;d@_�@_
=@_
=@^��@^�@^5?@]��@]�h@]/@\�/@\z�@[ƨ@[�F@[S�@[@Z�@Z��@Z�\@Zn�@Y�@Yx�@Y&�@X��@Xr�@XA�@W�;@W
=@V�R@VE�@V{@V@U@Up�@T��@T�@TZ@T1@Sƨ@SS�@S@R��@R�!@R�\@R�G�O�@P�`@P  @O��@O�@N��@N�+@Nff@N$�@M�@M�@M`B@M�@L�@L��@L�@L�@K�
@K��@K33@J�H@J��@J^5@I��@IG�G�O�@H��@G�@G�P@G;d@F��@F�+@FE�@F$�@F{@F@E�T@E�T@E@E�@E/@D�/@D�@DZ@D�@C��@C��@C33@Co@B�@B��@B^5@BM�@B=q@B�@A��@A�#@A�#@A��@A�^@A��@A��@Ax�@AG�@A&�@A%@@��@@�@@Q�@@ �@?�@?l�@?�@>�+@>ff@>V@>V@>V@>V@>E�@>{@=�@=��@=��@=�@=?}@<�@<�@<Z@;�F@;�@;dZG�O�@:�!@:J@9X@9&�@8��@8Ĝ@8�@8A�@8b@8  @7�;@7|�@7;d@7
=@6ȴ@6��@6{@5@5�-@5�h@5�G�O�@4�@4(�@3dZ@3@2��@2�!@2�!@2�!@2�!@2��@2��@2�\@2~�@2n�@1��@1�#@1�7@1hs@1hsG�O�@0��@0Q�@0A�@01'@0  @/�;@/��@.��@.��@.v�@.ff@.ff@.V@.E�@-�@-@-�h@-?}@,�j@,�@,��@,�D@,z�@,Z@,�@+ƨ@+��@+C�@*�@*�@*�!@*^5@*-@)�#@)��@)G�@)&�@)%@(�`@(�@(b@'�;@'��@'\)@'K�@'�@&��@&ȴG�O�@&v�@&$�@&@%@%�@%`B@%?}@%�@%V@$��G�O�@#�F@#�@#C�@"�H@"~�@"�@!��@!hs@!G�@!&�@ ��@ �u@ bN@ Q�G�O�G�O�@|�@�@5?@@�@p�@��@�D@j@I�@(�@1@�
@�
@�
@ƨ@S�@�@��@��G�O�@M�@�#@��@�7@G�@&�@�`@�9@�u@�@�@bN@�@�w@��@�P@|�@;d@��@�y@��@ffG�O�@��@O�@V@��@��@Z@(�@(�@�@�m@��@t�@S�@�@��@��@�!@^5@^5@^5@=q@��@�#@��@�^@��@hs@G�@7L@7L@7L@&�@&�@&�@�@��@bN@A�@1'@1'@1'@1'@ �@b@�@+@
=@�+@E�@�T@��@�/@��@��@z�@Z@9X@(�@�@��@ƨ@��@��@�@t�@S�@"�@
�@
��@
M�@
J@	��@	�^@	��@	hs@	7L@	�@	%@	%@�`@��@Ĝ@�9@��@�@bN@Q�@1'@1'@b@�;@�@�P@K�@
=@�y@ȴ@��@�+@v�@E�@$�@@@p�@O�@�@�@��@�D@I�@�@1@�m@�F@��@�@�@�@�@�@�@dZ@o@o@�@�!@�\@�\@�\11111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111113111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111141111111111111111111114111111111111111111141111111111111111111111111111111111111111111111114111111111141111111111111144111111111111111111114111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�sB�;B��B��B��B�3B�9B�XB�dB�dB�^B�RB�9B�?B�9B�FB�?B�!B��B��B��B�'B�-B�B��B�=Bo�Bt�B^5BD�B;dB,B(�B!�BuB�B �B�B
=B��B�B�fB�TB�/B��B�LB��Bx�B|�B�B�B}�Bv�By�Br�BcTBR�BN�B;dB�BB
�yB
��B
�}B
�B
��B
�bB
v�B
s�B
u�B
r�B
m�B
jB
\)B
N�B
7LB
&�B
!�B
�B
uB
hB
bB
bB
VB
DB
B
  B
  B	��B	��B	��B	��B	�B	�#B	ƨB	�dB	�B	�3B	�'B	��B	�3B	�-B	�B	��B	��B	�JB	�oB	��B	��B	�hB	�\B	�JB	�B	s�B	n�B	q�B	e`B	T�B	R�B	J�B	F�B	I�B	C�B	1'B	+B	2-B	49B	/B	.B	1'B	-B	)�B	$�B	�B	bB	hB	PB��B��B��B��B�B�sB�;B�NB�)B�)B�B�B�
B��B��BƨBƨB��BB��BÖB�}B�XB�^B�FB�!B�9B�LB�?B�FB�FB�'B�B�B�B��B��B�oB�1B�bB�VB�DB�B�B�+B�B�7B�JB�=B�+B�1B}�B~�B�B�%B�B�B|�Br�BbNBZBcTBbNBZB`BB]/B[#B[#BT�BI�B?}BM�BH�B?}BD�BC�B@�BA�BE�BF�BE�B>wB?}B>wB=qB1'B%�B)�B5?B=qB=qB?}B?}B?}B>wB<jB;dB:^B8RB6FB1'B/B1'B/B/B6FB5?B/B(�B-B&�B#�B)�B-B-B(�B%�B#�B�B�B�B �B"�B�B �B{B�B'�B#�B,B)�B'�B(�B�B�B�B �B#�B#�B,B(�B(�B)�B&�B$�B,B'�B&�B(�B)�B(�B1'B2-B1'B1'B/B7LB33B49B5?B<jB?}B>wB:^B=qB<jB?}B9XBC�BD�BC�BE�BF�BM�BI�BH�BP�BQ�BT�BXBYB_;B_;B_;B`BB`BB_;BffBiyBjBhsBk�Bl�Bp�Br�Bu�Bu�Bu�Bu�Bs�Bs�Bv�Bw�Bz�By�Bz�Bz�B~�B�B�B�B�%B�7B�VB�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�3B�FB�RB�FB�XB�jB�qB�qB�jB��BÖB��BŢB��BɺBɺB��B��BȴB��B��B��B��B��B�B�B�
B�B�)B�ZB�B�B�B��B��B��B��B��B��B��B��B��B	  B	B	DB	PB	\B	hB	bB	�B	�B	�B	�B	"�B	'�B	-B	1'B	33B	49B	49B	49B	5?B	6FB	6FB	;dB	>wB	@�B	@�B	B�B	A�B	B�B	I�B	O�B	S�B	S�B	R�B	VB	YB	YB	YB	ZB	\)B	\)B	_;B	ffB	o�B	r�B	w�B	y�B	z�B	{�B	{�B	|�B	|�B	|�B	|�B	}�B	}�B	�B	�B	�B	�%B	�1B	�7B	�JB	�PB	�PB	�VB	�\B	�VB	�\B	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�9B	�?B	�?B	�FB	�FB	�FB	�RB	�qB	��B	B	ĜB	ǮB	ƨB	ƨB	ƨB	ɺB	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�5B	�/B	�)B	�5B	�;B	�;B	�BB	�BB	�BB	�ZB	�ZB	�`B	�mB	�mB	�mB	�sB	�mB	�mB	�yB	�yB	�yB	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
  B
B
B
B
B
B
B
B
B
B
%B
+B
%B
B
1B

=B
1B
+B

=B
JB
PB
PB
\B
bB
hB
hB
bB
hB
bB
\B
bB
hB
hB
hB
oB
uB
uB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
&�B
(�B
(�B
(�B
'�B
)�B
,B
+B
+B
)�B
'�B
(�B
'�B
+B
-B
/B
/B
/B
/B
/B
0!B
0!B
0!B
/B
.B
0!B
0!B
2-B
2-B
1'B
/B
1'B
33B
33B
33B
33B
2-B
1'B
33B
6FB
7LB
6FB
6FB
6FB
5?B
5?B
6FB
5?B
5?B
8RB
9XB
9XB
9XB
8RB
7LB
7LB
8RB
8RB
8RB
:^B
9XB
9XB
:^B
:^B
:^B
;dB
<jB
<jB
<jB
;dB
;dB
=qB
>wB
>wB
?}B
?}B
?}B
>wB
@�B
>wB
>wB
@�B
?}B
@�B
A�B
A�B
A�B
A�B
@�B
?}B
<jB
A�B
A�B
@�B
A�B
B�B
B�B
C�B
E�B
D�B
C�B
D�B
E�B
F�B
F�B
D�B
B�B
B�B
C�B
E�B
H�B
H�B
G�B
G�B
K�B
K�B
K�B
K�B
K�B
M�B
L�B
K�B
J�B
J�B
M�B
M�B
M�B
K�B
L�B
N�B
O�B
O�B
P�B
P�B
P�B
Q�B
R�B
Q�B
Q�B
P�B
Q�B
R�B
S�B
R�B
R�B
R�B
S�B
R�B
R�B
S�B
Q�B
S�B
T�B
W
B
VB
VB
W
B
XB
XB
W
B
W
B
YB
YB
XB
YB
ZB
ZB
YB
[#B
[#B
ZB
ZB
ZB
\)B
[#B
[#B
[#B
\)B
\)B
]/B
]/B
]/B
\)B
\)B
[#B
ZB
[#B
]/B
^5B
_;B
^5B
^5B
^5B
]/B
\)B
\)B
^5B
]/B
^5B
^5B
^5B
]/B
`BB
cTB
bNB
bNB
bNB
bNB
cTB
bNB
bNB
cTB
dZB
cTB
dZB
cTB
cTB
bNB
cTB
bNB
cTB
dZB
e`B
e`B
dZB
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
hsB
hsB
hsB
hsB
gmB
hsB
hsB
hsB
hsB
iyB
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
k�B
k�B
l�B
k�B
k�B
l�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
n�B
m�B
o�B
n�B
n�B
o�B
q�B
q�B
q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�B��B��B��B��B��B�B��B��B��B��B�B��B�B��B��B��B�~B��B��B�B��B�XB�dB��B��B�lB��B�ZB�TB�`B�ZB��B��B��B�yB��B��B��B��B�~Bq�Bu�B`vBG_B=�B.�B+6B$&BBB!�B�B0B�(B�tB�>B�B�BΊB�rB�B}�B.B�mB�GB~�Bw�BzDBs�Bd�BT�BPB=�B=BB
�wB
ѝB
�aB
��B
�@B
�[B
y�B
u�B
v�B
s�B
ncB
kQB
]�B
Q B
:*B
)�B
$B
�B
B
�B
4B
�B
�B
�B
B
 �B
 �B	��B	�XB	�FB	��B	�WB	��B	�7B	�B	�B	�TB	�GB	�QB	��B	��B	��B	��B	�VB	��B	�[B	��B	��B	�TB	��B	�B	�mB	u�B	p!B	r|B	g8B	W?B	T{B	L�B	G�B	JrB	D�B	3�B	,�B	3MB	4�B	0!B	.�B	1�B	-wB	*�B	%�B	7B	B	:B	<B�B�>B��B��B��B��B�B�B��B�dB�QB�	B�+B�:B�"B��BǮBªB�{B� B��B�4B�^B��B�LB�B��B��B��B��B�B�|B�;B�"B��B��B�VB�{B�XB�NB�(B��B�SB��B�B�MB��B�~B��B��B�B�B�4B��B�tB�{B�oB}�Bs�Bd�B\CBd�Bc�B\BaHB^�B\�B\CBV�BK�BBBN�BJXBA�BFBEBB'BB�BFtBG�BF�B?�B@OB?�B>wB2�B(�B+�B6FB=�B=�B?�B?�B?�B>�B<�B;�B:�B8�B6�B2B0B1�B0!B/�B6�B5�B0B*B-�B($B%�B*�B-�B-�B)�B&�B$�B=BB	B!�B#�B �B!�B�B 'B(�B$�B,�B*�B(�B)yB!-BCB5B"4B%,B$�B,�B)�B)�B*�B(
B%�B,�B(�B'�B)�B+B*B1�B2�B1�B2B0!B7�B4B5%B6FB<�B?�B>�B;JB>(B=<B@4B:�BDBE9BDMBFtBGzBN<BJ�BI�BQ�BR�BU�BX�BY�B_�B_�B_�B`�B`�B`'Bf�Bi�Bj�BiBlBm)Bp�BsBu�Bu�BvBvBt9BtTBw2BxRB{JBz^B{B{�B�B��B��B��B��B��B��B�B��B�B��B��B��B�B�QB�B�4B�@B�:B�hB�`B��B��B�hB�tB��B�zB��B��B��B��B��B��B�B��B��B�B�B��B�	B�	B��B�B�RB�.B�,B�[B�TB�oB�_B�yB׍B��B��B��B�B��B�!B�	B��B�B�B�0B�0B�B�lB�qB	 iB	�B	�B	�B	�B	�B	�B	�B	�B	$B	!B	# B	(>B	-]B	1[B	3hB	4nB	4nB	4nB	5tB	6�B	6�B	;�B	>�B	@�B	@�B	B�B	A�B	B�B	J#B	PB	TB	T,B	S@B	VSB	YKB	YKB	YeB	ZkB	\CB	\]B	_�B	f�B	o�B	r�B	w�B	y�B	z�B	|B	|B	}B	}B	|�B	}"B	~(B	~BB	� B	�MB	�mB	�tB	�fB	�lB	�~B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�FB	�`B	�tB	�RB	�XB	�QB	�CB	�IB	�IB	�;B	�[B	�aB	�TB	�nB	�tB	�tB	�zB	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�)B	�6B	��B	�B	��B	�2B	�,B	�2B	�2B	�9B	�9B	�?B	�EB	�KB	�QB	�QB	�eB	�B	�qB	�]B	�jB	�pB	�VB	�jB	�dB	ܒB	�jB	�VB	ߊB	�vB	��B	�B	�B	�B	�B	�mB	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	�B	�2G�O�B	�LB	�"B	�6B	�<B
 B
 4B
 4B
 B
 OB
AB
AB
-B
MB
-B
[B
MB
SB
mB
YB
_B
tB
�B
fB

rG�O�B
�B

rB
~B
jB
�B
vB
�B
�B
�B
�B
hB
}B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
B
B
!�B
"B
!�G�O�B
 B
!B
%B
%B
$�B
%,B
&B
'B
'B
'B
'B
'B
)*B
)*B
)*B
(>B
*0B
,"B
+6B
+6B
*KG�O�B
)DB
(XB
+6B
-)B
/5B
/5B
/5B
/B
/5B
0;B
0;B
0UB
/OB
.cB
0UB
0;B
2GB
2GB
1[G�O�B
1[B
3MB
3hB
3hB
3hB
2aB
1vB
3hB
6`B
7fB
6`B
6`B
6zB
5tB
5tB
6zB
5tB
5tB
8lB
9rB
9rB
9rB
8�B
7�B
7�B
8�B
8lB
8�B
:xB
9�B
9�B
:�B
:�B
:�B
;�B
<�B
<�B
<�B
;�B
;�B
=�B
>�B
>�B
?�B
?�B
?�B
>�B
@�G�O�B
>�B
@�B
?�B
@�B
A�B
A�B
A�B
A�B
@�B
?�G�O�B
A�B
A�B
@�B
A�B
B�B
B�B
C�B
E�B
D�B
C�B
D�B
E�B
F�B
F�G�O�G�O�B
B�B
C�B
E�B
H�B
H�B
G�B
G�B
K�B
K�B
K�B
K�B
K�B
M�B
L�B
K�B
KB
J�B
M�B
NB
NG�O�B
MB
N�B
PB
O�B
QB
QB
QB
R B
SB
Q�B
R B
Q4B
R B
S&B
TB
S&B
S&B
S&B
T,B
SB
S&B
T,G�O�B
T,B
U2B
W
B
V9B
V9B
W$B
X+B
XB
W?B
W?B
YKB
Y1B
XEB
YKB
Z7B
Z7B
YKB
[=B
[=B
ZQB
Z7B
ZQB
\CB
[WB
[qB
[WB
\)B
\CB
]IB
]IB
]IB
\CB
\)B
[=B
ZQB
[qB
]dB
^5B
_VB
^OB
^OB
^OB
]dB
\xB
\xB
^OB
]~B
^jB
^�B
^jB
]�B
`vB
cnB
b�B
bhB
b�B
b�B
cTB
b�B
bhB
c�B
dZB
c�B
dtB
cnB
c�B
b�B
c�B
b�B
c�B
d�B
ezB
e�B
d�B
e�B
f�B
f�B
ffB
f�B
f�B
g�B
g�B
gmB
g�B
g�B
hsB
h�B
hsB
h�B
g�B
h�B
h�B
h�B
h�B
i�B
i�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
l�B
k�B
k�B
l�B
m�B
m�B
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
n�B
m�B
o�B
n�B
n�B
o�B
q�B
q�B
q�11111111111111111111111111111111111111111111111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111113111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111141111111111111111111114111111111111111111141111111111111111111111111111111111111111111111114111111111141111111111111144111111111111111111114111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
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
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.19(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201805180035352018051800353520180518003535201806221241532018062212415320180622124153201806042120222018060421202220180604212022  JA  ARFMdecpA19c                                                                20180514093551  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180514003558  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180514003601  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180514003601  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180514003602  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180514003602  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180514003602  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180514003602  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180514003604  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180514003604                      G�O�G�O�G�O�                JA  ARUP                                                                        20180514005712                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180514153150  CV  JULD            G�O�G�O�F��                JM  ARSQJMQC2.0                                                                 20180515000000  CF  PSAL_ADJUSTED_QCC  D�� G�O�                JM  ARSQJMQC2.0                                                                 20180515000000  CF  TEMP_ADJUSTED_QCC  D�� G�O�                JM  ARCAJMQC2.0                                                                 20180517153535  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180517153535  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180604122022  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622034153  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131518                      G�O�G�O�G�O�                