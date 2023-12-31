CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-11-25T08:01:05Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �T   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݼ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20211125080105  20211125080105  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               }A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @٥Sr���1   @٥T�d�@<"��`A��c���v�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         }A   A   F   @���@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�D�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@��R@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{��D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�6�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�DڽD���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�6�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�C�D�`R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AÏ\AÍPAÉ7AÉ7Aã�AøRAøRAò-Aô9AîAöFAò-Að!Að!Að!AöFAð!Aò-Að!Aò-AöFAøRAô9Aô9AöFAþwA�ĜA���A��A��TAé�A�E�A�A�r�A��A�bA�33A�7LA�(�A�-A�+A���A�`BA�hsA��wA�M�A��A��!A�&�A�A�O�A��;A���A�/A�VA��+A�ȴA���A��A�r�A�{A��mA��jA���A�oA�VA��\A���A��wA�jA��A�\)A��A�1'A��A�`BA���A�/A��A��`A���A�A��-A�r�A�n�A���A�`BA�n�A�A�$�A�A���A�M�A�A��!A}��Ax�DAt�jAq�Ao��Al�Aj  Ah��Ag��Af�jAdr�Ab�A`n�A^jA]G�A\�9A[XAY�AYdZAX��AWhsAVJAT�ATM�AS��AR�`AR��ARM�AP�/ANz�AL��AJ��AJE�AJ(�AI�FAHffAGK�AFffAEO�AC�-AB�/AB=qAAt�A@I�A?hsA>��A>�!A>E�A=��A<�`A;XA:A�A9ƨA9hsA8��A8(�A7��A7+A65?A5ƨA4��A4z�A4I�A3��A1��A1dZA1�A0JA/��A/"�A/+A.�A-�TA,z�A+7LA*�A)\)A(�+A'��A%A%;dA$$�A#�-A#�PA#dZA"��A"  A!p�A �`A ��A�
A&�AQ�A�AjA�hA|�Al�A`BA?}A�AȴA�mAx�A�A|�A�`AĜA�!A�\A{AdZA�+A�A��A�+AA�wAO�AffA��At�A�AbA�hA
�!A	��A	hsAffAAx�A��A�wA;dA�RA^5A�#A33AE�A7L@�K�@��\@�5?@��@�O�@��9@�l�@���@�&�@��;@��y@���@��@�9X@��@�?}@��@��@�ff@�{@�hs@�Q�@�M�@�j@��@�K�@�O�@�|�@�^5@�?}@��/@�b@��H@�$�@��`@�Q�@�;d@�E�@ّh@؋D@�+@�?}@ӶF@�@�V@�j@υ@�@���@�(�@���@���@���@��@�-@��T@�G�@î@�"�@�@�X@�z�@��@���@�o@���@�=q@���@�%@�Z@��w@�V@��T@���@���@�(�@�+@��!@��@���@��@�9X@�v�@���@��^@�p�@�Z@�dZ@�K�@�;d@�o@�n�@�@�hs@�7L@���@�r�@��@��@��@�hs@�V@���@�9X@�C�@�ȴ@�5?@���@�X@��/@��@�A�@��
@��F@��@�
=@�^5@���@��h@��u@��m@�;d@��@��y@��H@���@���@�{@���@�&�@��@�I�@��@�+@�
=@�ȴ@�$�@��#@��@�X@�?}@��@�Ĝ@�bN@���@���@�;d@��!@�-@�@�O�@�7L@��@��@��;@��@��R@���@�$�@��^@�hs@���@��@���@�A�@��
@���@�ƨ@�t�@�@�ȴ@�M�@�@��#@��7@�X@���@��@��@�A�@��;@���@�C�@��@���@�@�G�@��@��j@�9X@���@��P@�t�@�C�@���@��@���@�-@�@��#@���@�G�@�&�@���@��@��m@��P@�S�@���@��R@�ff@�M�@�-@��^@�X@���@��9@���@��u@�z�@�Q�@� �@�  @�w@~ȴ@~5?@}@}�@|�j@|z�@|(�@{��@z�@z�H@z��@z��@z~�@zn�@zn�@zM�@z�@y�@yhs@yG�@y&�@x�@w�@w�;@w�;@w�w@w;d@v�+@vff@vE�@vE�@vE�@v$�@u�T@u�-@u?}@t�D@t(�@t(�@s��@s33@r~�@q�@qx�@p�@o��@o
=@o
=@o
=@n��@n��@nȴ@nff@nV@n{@m��@m/@l��@k�F@j��@jJ@i��@i7L@i%@h��@hĜ@hbN@hA�@hA�@h1'@g�@g�@g;d@f��@f�+@fV@e�T@e��@e?}@d�/@d�j@d�@dj@cƨ@c��@c��@cS�@c@b��@b�\@a�7@`Ĝ@_�@_��@_;d@^ȴ@]@]p�@]/@]V@]V@\z�@[�m@["�@[o@[@[@Z��@Z�\@Zn�@ZJ@Y�#@Y��@Y�7@Yhs@YG�@Y�@Y%@X�`@X��@X�u@X�u@X�@Xr�@XQ�@X1'@W��@W\)@W�@Vȴ@Vff@U��@UV@T�j@T��@Tz�@TZ@T�@S�m@S�
@St�@S@R��@R��@R~�@RM�@R=q@RJ@Q��@Q�7@Qx�@QX@P�9@PA�@O�;@O�P@O\)@O
=@N�@Nȴ@NE�@M�-@M�@M�@Mp�@M`B@M`B@M`B@MO�@MO�@MO�@M?}@M/@M�@MV@L��@L�/@L�@Lj@L9X@L1@K�F@KdZ@J�@Jn�@J-@I�@I�7@I�@H��@HA�@G�;@G�P@Gl�@Gl�@G\)@G;d@G+@G�@G
=@F��@F�R@F��@F{@E@E@E@E�-@E�-@E�@D�/@D�D@D(�@Cƨ@C"�@B��@B�!@B-@A��@A�^@A��@A�7@AG�@@��@@�@@Q�@@Q�@@ �@?K�@?
=@>�@>��@>E�@>{@=�T@=@=�@=?}@<�j@<z�@<Z@<(�@<1@;�F@;C�@;@:��@:^5@9��@9��@9�7@9X@9G�@9�@8Ĝ@8r�@8  @7�@7l�@7+@6�y@6�R@6�+@6E�@5��@5�@4�@4j@49X@4(�@3�
@3��@3�@3dZ@3C�@333@3o@2��@2=q@1��@1�7@0�`@0�u@0bN@01'@/�;@/��@/|�@/K�@.��@.V@.$�@.@-�T@-��@-@-�h@-p�@,��@,�@,��@,I�@+��@+�
@+�F@+t�@+t�@+C�@+"�@+@*�@*�H@*��@*�\@*^5@*�@)�#@)�#@)�#@)�^@)�7@)G�@)%@(�9@(��@(bN@(b@'�;@'�@'�P@'|�@'|�@'\)@&��@&�y@&��@&v�@&5?@%@%p�@%`B@%?}@$�j@$�j@$�@$j@$9X@$1@#C�@#o@#@"��@"�!@"��@"��@"�\@"�\@"M�@"�@!�#@!�^@!X@!7L@!�@ ��@ ��@ ��@ Ĝ@ ��@ �u@ Q�@ 1'@ b@��@�@|�@\)@�@�y@��@V@{@@�@@`B@�@�@��@�@(�@1@�m@��@��@t�@S�@"�@�H@��@�\@n�@-@�@�@�@�7@�@�`@�9@bN@ �@�;@�@�P@l�@+@�y@�R@�+@ff@E�@@�T@@��@`B@/@��@�/@�j@��@z�@I�@(�@(�@�@1@�m@ƨ@ƨ@�F@��@��@dZ@@�H@�!@~�@n�@M�@�@��@��@�7@hs@7L@%@��@Ĝ@r�@A�@ �@�;@��@�w@�@��@�P@\)@;d@�@�@��@��@�+@ff@5?@{@@�@��@�-@��@��@`B@?}@�@V@V@�@j@j@9X@1@�m@�
@�F@��@�@S�@o@
�H@
�!@
��@
��@
~�@
n�@
M�@
J@	�@	��@	X@	G�@	G�@	7L@	7L@	�@Ĝ@��@�@A�@b@�;@�@��@�P@��@�P@|�@+@�y@��@E�@{@�@@�h@p�@p�@p�@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AÏ\AÍPAÉ7AÉ7Aã�AøRAøRAò-Aô9AîAöFAò-Að!Að!Að!AöFAð!Aò-Að!Aò-AöFAøRAô9Aô9AöFAþwA�ĜA���A��A��TAé�A�E�A�A�r�A��A�bA�33A�7LA�(�A�-A�+A���A�`BA�hsA��wA�M�A��A��!A�&�A�A�O�A��;A���A�/A�VA��+A�ȴA���A��A�r�A�{A��mA��jA���A�oA�VA��\A���A��wA�jA��A�\)A��A�1'A��A�`BA���A�/A��A��`A���A�A��-A�r�A�n�A���A�`BA�n�A�A�$�A�A���A�M�A�A��!A}��Ax�DAt�jAq�Ao��Al�Aj  Ah��Ag��Af�jAdr�Ab�A`n�A^jA]G�A\�9A[XAY�AYdZAX��AWhsAVJAT�ATM�AS��AR�`AR��ARM�AP�/ANz�AL��AJ��AJE�AJ(�AI�FAHffAGK�AFffAEO�AC�-AB�/AB=qAAt�A@I�A?hsA>��A>�!A>E�A=��A<�`A;XA:A�A9ƨA9hsA8��A8(�A7��A7+A65?A5ƨA4��A4z�A4I�A3��A1��A1dZA1�A0JA/��A/"�A/+A.�A-�TA,z�A+7LA*�A)\)A(�+A'��A%A%;dA$$�A#�-A#�PA#dZA"��A"  A!p�A �`A ��A�
A&�AQ�A�AjA�hA|�Al�A`BA?}A�AȴA�mAx�A�A|�A�`AĜA�!A�\A{AdZA�+A�A��A�+AA�wAO�AffA��At�A�AbA�hA
�!A	��A	hsAffAAx�A��A�wA;dA�RA^5A�#A33AE�A7L@�K�@��\@�5?@��@�O�@��9@�l�@���@�&�@��;@��y@���@��@�9X@��@�?}@��@��@�ff@�{@�hs@�Q�@�M�@�j@��@�K�@�O�@�|�@�^5@�?}@��/@�b@��H@�$�@��`@�Q�@�;d@�E�@ّh@؋D@�+@�?}@ӶF@�@�V@�j@υ@�@���@�(�@���@���@���@��@�-@��T@�G�@î@�"�@�@�X@�z�@��@���@�o@���@�=q@���@�%@�Z@��w@�V@��T@���@���@�(�@�+@��!@��@���@��@�9X@�v�@���@��^@�p�@�Z@�dZ@�K�@�;d@�o@�n�@�@�hs@�7L@���@�r�@��@��@��@�hs@�V@���@�9X@�C�@�ȴ@�5?@���@�X@��/@��@�A�@��
@��F@��@�
=@�^5@���@��h@��u@��m@�;d@��@��y@��H@���@���@�{@���@�&�@��@�I�@��@�+@�
=@�ȴ@�$�@��#@��@�X@�?}@��@�Ĝ@�bN@���@���@�;d@��!@�-@�@�O�@�7L@��@��@��;@��@��R@���@�$�@��^@�hs@���@��@���@�A�@��
@���@�ƨ@�t�@�@�ȴ@�M�@�@��#@��7@�X@���@��@��@�A�@��;@���@�C�@��@���@�@�G�@��@��j@�9X@���@��P@�t�@�C�@���@��@���@�-@�@��#@���@�G�@�&�@���@��@��m@��P@�S�@���@��R@�ff@�M�@�-@��^@�X@���@��9@���@��u@�z�@�Q�@� �@�  @�w@~ȴ@~5?@}@}�@|�j@|z�@|(�@{��@z�@z�H@z��@z��@z~�@zn�@zn�@zM�@z�@y�@yhs@yG�@y&�@x�@w�@w�;@w�;@w�w@w;d@v�+@vff@vE�@vE�@vE�@v$�@u�T@u�-@u?}@t�D@t(�@t(�@s��@s33@r~�@q�@qx�@p�@o��@o
=@o
=@o
=@n��@n��@nȴ@nff@nV@n{@m��@m/@l��@k�F@j��@jJ@i��@i7L@i%@h��@hĜ@hbN@hA�@hA�@h1'@g�@g�@g;d@f��@f�+@fV@e�T@e��@e?}@d�/@d�j@d�@dj@cƨ@c��@c��@cS�@c@b��@b�\@a�7@`Ĝ@_�@_��@_;d@^ȴ@]@]p�@]/@]V@]V@\z�@[�m@["�@[o@[@[@Z��@Z�\@Zn�@ZJ@Y�#@Y��@Y�7@Yhs@YG�@Y�@Y%@X�`@X��@X�u@X�u@X�@Xr�@XQ�@X1'@W��@W\)@W�@Vȴ@Vff@U��@UV@T�j@T��@Tz�@TZ@T�@S�m@S�
@St�@S@R��@R��@R~�@RM�@R=q@RJ@Q��@Q�7@Qx�@QX@P�9@PA�@O�;@O�P@O\)@O
=@N�@Nȴ@NE�@M�-@M�@M�@Mp�@M`B@M`B@M`B@MO�@MO�@MO�@M?}@M/@M�@MV@L��@L�/@L�@Lj@L9X@L1@K�F@KdZ@J�@Jn�@J-@I�@I�7@I�@H��@HA�@G�;@G�P@Gl�@Gl�@G\)@G;d@G+@G�@G
=@F��@F�R@F��@F{@E@E@E@E�-@E�-@E�@D�/@D�D@D(�@Cƨ@C"�@B��@B�!@B-@A��@A�^@A��@A�7@AG�@@��@@�@@Q�@@Q�@@ �@?K�@?
=@>�@>��@>E�@>{@=�T@=@=�@=?}@<�j@<z�@<Z@<(�@<1@;�F@;C�@;@:��@:^5@9��@9��@9�7@9X@9G�@9�@8Ĝ@8r�@8  @7�@7l�@7+@6�y@6�R@6�+@6E�@5��@5�@4�@4j@49X@4(�@3�
@3��@3�@3dZ@3C�@333@3o@2��@2=q@1��@1�7@0�`@0�u@0bN@01'@/�;@/��@/|�@/K�@.��@.V@.$�@.@-�T@-��@-@-�h@-p�@,��@,�@,��@,I�@+��@+�
@+�F@+t�@+t�@+C�@+"�@+@*�@*�H@*��@*�\@*^5@*�@)�#@)�#@)�#@)�^@)�7@)G�@)%@(�9@(��@(bN@(b@'�;@'�@'�P@'|�@'|�@'\)@&��@&�y@&��@&v�@&5?@%@%p�@%`B@%?}@$�j@$�j@$�@$j@$9X@$1@#C�@#o@#@"��@"�!@"��@"��@"�\@"�\@"M�@"�@!�#@!�^@!X@!7L@!�@ ��@ ��@ ��@ Ĝ@ ��@ �u@ Q�@ 1'@ b@��@�@|�@\)@�@�y@��@V@{@@�@@`B@�@�@��@�@(�@1@�m@��@��@t�@S�@"�@�H@��@�\@n�@-@�@�@�@�7@�@�`@�9@bN@ �@�;@�@�P@l�@+@�y@�R@�+@ff@E�@@�T@@��@`B@/@��@�/@�j@��@z�@I�@(�@(�@�@1@�m@ƨ@ƨ@�F@��@��@dZ@@�H@�!@~�@n�@M�@�@��@��@�7@hs@7L@%@��@Ĝ@r�@A�@ �@�;@��@�w@�@��@�P@\)@;d@�@�@��@��@�+@ff@5?@{@@�@��@�-@��@��@`B@?}@�@V@V@�@j@j@9X@1@�m@�
@�F@��@�@S�@o@
�H@
�!@
��@
��@
~�@
n�@
M�@
J@	�@	��@	X@	G�@	G�@	7L@	7L@	�@Ĝ@��@�@A�@b@�;@�@��@�P@��@�P@|�@+@�y@��@E�@{@�@@�h@p�@p�@p�@p�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�FB�?B�9B�9B�dBBÖBÖBÖBĜBŢBĜBĜBŢBŢBŢBƨBƨBƨBǮBǮBȴBǮBǮBȴB��B��B��B�
B�;B�B�B�yB��B��B�DBp�BT�BoB��B�B�B�B�mB�TB�BB�)B�#B�
B��BƨBB��B�jB�XB�LB��B�+B{�Bq�Bk�BhsBffBcTB\)BQ�B:^B-B(�B"�B�BbB��B�B�`B��B�FB��B�1Bz�BiyB[#BT�BO�BA�B,B�B+B��B�B�NB�B�B��BɺB�-B�PBs�B^5BN�B>wB/B'�B�B�BVBB��B�B�fB�TB�5B�
B��B��BɺBŢB�wB�dB�RB�FB�9B�'B�B��B��B��B��B�{B�oB�VB�7B�%B�B� B|�B{�Bu�Br�Bo�Bm�Bl�Bk�BiyBgmBdZB`BB^5B^5B[#BZBZBW
BT�BS�BR�BP�BQ�BVBQ�BM�BM�BJ�BG�BF�BF�BE�BC�B=qB9XB33B0!B,B)�B�B�B�B�B�B�B�B�B{BoBhB\BVBDB
=B1B+B%B%BBBBB
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
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�sB
�mB
�`B
�`B
�NB
�HB
�BB
�;B
�5B
�)B
�#B
�B
�B
�B
�B
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
ȴB
ȴB
ǮB
ǮB
ƨB
ĜB
ĜB
ĜB
B
B
B
��B
��B
��B
��B
�}B
�wB
�}B
�wB
�wB
�wB
�qB
�qB
�wB
�qB
�wB
�qB
�qB
�jB
�jB
�dB
�dB
�dB
�dB
�jB
�qB
�}B
�}B
�}B
�}B
�wB
�qB
�wB
�wB
��B
��B
��B
B
ȴB
ȴB
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
�B
�B
�B
�B
�/B
�;B
�BB
�`B
�mB
�mB
�yB
�B
�B
�B
�B
�B
��B
�B
�B
�B
��B
��B
��B
��B
��B  BBB	7BJBVBPBVBhBuB�B�B�B�B�B �B"�B"�B#�B$�B(�B-B.B33B6FB:^B<jB<jB<jB=qB>wB@�BC�BF�BI�BJ�BO�BQ�BR�BS�BXBZB\)B]/B]/B^5B`BBbNBffBhsBiyBk�Bn�Bp�Bs�Bs�Bu�Bw�B{�B�B�B�B�%B�7B�DB�VB�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�?B�LB�^B�jB�qBBǮBȴB��B��B��B��B��B�
B�B�B�#B�;B�HB�NB�ZB�sB�yB�B�B�B�B��B��B��B��B  BBB%B	7BDBDBDBJBPBVBbBhB�B�B�B�B �B!�B$�B'�B+B,B.B0!B1'B2-B1'B1'B1'B2-B49B5?B5?B8RB;dB;dB;dB<jB?}BD�BD�BE�BE�BF�BF�BG�BH�BI�BK�BM�BO�BP�BS�BVBVBW
BYB\)B_;B_;B_;B_;B_;B`BBbNBbNBcTBdZBffBgmBiyBm�Bo�Bq�Br�Bs�Bs�Bs�Bu�Bu�Bu�Bu�Bv�Bw�Bz�B{�B|�B|�B~�B� B�B�B�B�B�B�+B�+B�1B�7B�=B�DB�DB�\B�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�-B�3B�9B�?B�LB�XB�^B�^B�dB�dB�jB�jB�jB�qB�}B�}B��B��B��B��BBÖBÖBÖBÖBŢBƨBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�
B�B�B�B�#B�)B�/B�5B�5B�5B�5B�5B�5B�;B�;B�;B�;B�HB�NB�NB�NB�NB�NB�NB�ZB�`B�fB�mB�mB�mB�mB�sB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBBBBBBBBB%B+B1B1B
=B
=B
=BDBDBJBJBJBPBVBVB\B\B\B\B\BbBbBhBhBoBoBoBuBuBuBuB{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B!�B!�B!�B!�B"�B"�B"�B"�B"�B"�B#�B#�B$�B$�B$�B$�B$�B$�B%�B%�B%�B%�B&�B&�B&�B&�B'�B'�B'�B'�B(�B(�B)�B)�B)�B)�B+B+B+B+B,B-B-B-B-B.B.B.B.B/B/B/B/B0!B0!B0!B0!B1'B2-B2-B2-B33B33B33B49B49B49B49B5?B5?B5?B5?B5?B6FB6FB6FB6FB7LB7LB7LB7LB7LB8RB8RB8RB8RB8RB9XB9XB9XB9XB9XB9XB9XB9XB:^B:^B:^B;dB;dB;dB;dB;dB<jB<jB<jB<jB=qB=qB=qB=qB>wB>wB>wB?}B?}B?}B?}B?}B@�B@�B@�B@�B@�B@�BA�BA�BB�BB�BB�BB�BB�BB�BC�BC�BC�BC�BC�BD�BD�BD�BD�BE�BD�BE�BE�BE�BE�BF�BF�BF�BF�BG�BG�BG�BG�BG�BH�BH�BH�BH�BH�BI�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BL�BL�BL�BL�BL�BL�BL�BM�BM�BM�BN�BN�BN�BO�BO�BO�BO�BO�BO�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B�FB�?B�9B�9B�dBBÖBÖBÖBĜBŢBĜBĜBŢBŢBŢBƨBƨBƨBǮBǮBȴBǮBǮBȴB��B��B��B�
B�;B�B�B�yB��B��B�DBp�BT�BoB��B�B�B�B�mB�TB�BB�)B�#B�
B��BƨBB��B�jB�XB�LB��B�+B{�Bq�Bk�BhsBffBcTB\)BQ�B:^B-B(�B"�B�BbB��B�B�`B��B�FB��B�1Bz�BiyB[#BT�BO�BA�B,B�B+B��B�B�NB�B�B��BɺB�-B�PBs�B^5BN�B>wB/B'�B�B�BVBB��B�B�fB�TB�5B�
B��B��BɺBŢB�wB�dB�RB�FB�9B�'B�B��B��B��B��B�{B�oB�VB�7B�%B�B� B|�B{�Bu�Br�Bo�Bm�Bl�Bk�BiyBgmBdZB`BB^5B^5B[#BZBZBW
BT�BS�BR�BP�BQ�BVBQ�BM�BM�BJ�BG�BF�BF�BE�BC�B=qB9XB33B0!B,B)�B�B�B�B�B�B�B�B�B{BoBhB\BVBDB
=B1B+B%B%BBBBB
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
�B
�B
�B
�B
�B
�B
�B
�B
�yB
�sB
�mB
�`B
�`B
�NB
�HB
�BB
�;B
�5B
�)B
�#B
�B
�B
�B
�B
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
ȴB
ȴB
ǮB
ǮB
ƨB
ĜB
ĜB
ĜB
B
B
B
��B
��B
��B
��B
�}B
�wB
�}B
�wB
�wB
�wB
�qB
�qB
�wB
�qB
�wB
�qB
�qB
�jB
�jB
�dB
�dB
�dB
�dB
�jB
�qB
�}B
�}B
�}B
�}B
�wB
�qB
�wB
�wB
��B
��B
��B
B
ȴB
ȴB
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
�B
�B
�B
�B
�/B
�;B
�BB
�`B
�mB
�mB
�yB
�B
�B
�B
�B
�B
��B
�B
�B
�B
��B
��B
��B
��B
��B  BBB	7BJBVBPBVBhBuB�B�B�B�B�B �B"�B"�B#�B$�B(�B-B.B33B6FB:^B<jB<jB<jB=qB>wB@�BC�BF�BI�BJ�BO�BQ�BR�BS�BXBZB\)B]/B]/B^5B`BBbNBffBhsBiyBk�Bn�Bp�Bs�Bs�Bu�Bw�B{�B�B�B�B�%B�7B�DB�VB�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�-B�?B�LB�^B�jB�qBBǮBȴB��B��B��B��B��B�
B�B�B�#B�;B�HB�NB�ZB�sB�yB�B�B�B�B��B��B��B��B  BBB%B	7BDBDBDBJBPBVBbBhB�B�B�B�B �B!�B$�B'�B+B,B.B0!B1'B2-B1'B1'B1'B2-B49B5?B5?B8RB;dB;dB;dB<jB?}BD�BD�BE�BE�BF�BF�BG�BH�BI�BK�BM�BO�BP�BS�BVBVBW
BYB\)B_;B_;B_;B_;B_;B`BBbNBbNBcTBdZBffBgmBiyBm�Bo�Bq�Br�Bs�Bs�Bs�Bu�Bu�Bu�Bu�Bv�Bw�Bz�B{�B|�B|�B~�B� B�B�B�B�B�B�+B�+B�1B�7B�=B�DB�DB�\B�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�!B�-B�3B�9B�?B�LB�XB�^B�^B�dB�dB�jB�jB�jB�qB�}B�}B��B��B��B��BBÖBÖBÖBÖBŢBƨBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�
B�B�B�B�#B�)B�/B�5B�5B�5B�5B�5B�5B�;B�;B�;B�;B�HB�NB�NB�NB�NB�NB�NB�ZB�`B�fB�mB�mB�mB�mB�sB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  BBBBBBBBBBBBBBB%B+B1B1B
=B
=B
=BDBDBJBJBJBPBVBVB\B\B\B\B\BbBbBhBhBoBoBoBuBuBuBuB{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B!�B!�B!�B!�B"�B"�B"�B"�B"�B"�B#�B#�B$�B$�B$�B$�B$�B$�B%�B%�B%�B%�B&�B&�B&�B&�B'�B'�B'�B'�B(�B(�B)�B)�B)�B)�B+B+B+B+B,B-B-B-B-B.B.B.B.B/B/B/B/B0!B0!B0!B0!B1'B2-B2-B2-B33B33B33B49B49B49B49B5?B5?B5?B5?B5?B6FB6FB6FB6FB7LB7LB7LB7LB7LB8RB8RB8RB8RB8RB9XB9XB9XB9XB9XB9XB9XB9XB:^B:^B:^B;dB;dB;dB;dB;dB<jB<jB<jB<jB=qB=qB=qB=qB>wB>wB>wB?}B?}B?}B?}B?}B@�B@�B@�B@�B@�B@�BA�BA�BB�BB�BB�BB�BB�BB�BC�BC�BC�BC�BC�BD�BD�BD�BD�BE�BD�BE�BE�BE�BE�BF�BF�BF�BF�BG�BG�BG�BG�BG�BH�BH�BH�BH�BH�BI�BJ�BJ�BJ�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BL�BL�BL�BL�BL�BL�BL�BM�BM�BM�BN�BN�BN�BO�BO�BO�BO�BO�BO�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211125080105                              AO  ARCAADJP                                                                    20211125080105    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211125080105  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211125080105  QCF$                G�O�G�O�G�O�8000            