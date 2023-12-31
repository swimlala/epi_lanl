CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-11-15T10:01:05Z creation      
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
resolution        =���   axis      Z        |  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \H   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     |  `(   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �      TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  �4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȱ   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     |  ̐   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20211115100105  20211115100105  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               |A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @٢�X^z1   @٢���UF@<	x����c��"��`1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         |A   A   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(�C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� DsfDs� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|fD|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��R@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'��B/=qB7=qB?=qBG=qBO=qBV�B_=qBg=qBo=qBw=qB=qB���B���B�k�B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B�k�Bߞ�B㞸B瞸B랸BB�B�k�B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'��C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D �qDmqD��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?�qD@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr�=Dss�Ds��Dts�Dt��Dus�Du��Dvs�Dv��Dws�Dw��Dxs�Dx��Dys�Dy��Dzs�Dz��D{s�D{�=D|s�D|��D}s�D}��D~s�D~��Ds�D��D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D¹�D���D�9�D�y�Dù�D���D�9�D�y�DĹ�D���D�9�D�y�DŹ�D���D�9�D�y�Dƹ�D���D�9�D�y�Dǹ�D���D�9�D�y�Dȹ�D���D�9�D�y�Dɹ�D���D�9�D�y�Dʹ�D���D�9�D�y�D˹�D���D�9�D�y�D̹�D���D�9�D�y�D͹�D���D�9�D�y�Dι�D���D�9�D�y�DϹ�D���D�9�D�y�Dй�D���D�9�D�y�Dѹ�D���D�9�D�y�Dҹ�D���D�9�D�y�Dӹ�D���D�9�D�y�DԹ�D���D�9�D�y�Dչ�D���D�9�D�y�Dֹ�D���D�9�D�y�D׹�D���D�9�D�y�Dع�D���D�9�D�y�Dٹ�D���D�9�D�y�Dڹ�D���D�9�D�y�D۹�D���D�9�D�y�Dܹ�D���D�9�D�y�Dݹ�D���D�9�D�y�D޹�D���D�9�D�y�D߹�D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D��D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D��D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D���D���D�9�D�y�D��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�%A�bA��A��A��A��A��A��A��A��A��A��A��A��A� �A�$�A�$�A�&�A�&�A�&�A�-A�/A�-A� �A��A��A�JA�{AļjAčPA�~�A���A��A�t�A���A�1A��^A��/A�bA��wA�
=A�JA���A��\A�A���A���A�;dA�~�A���A�&�A���A��A�ȴA��A��9A�5?A��#A�G�A��DA��yA���A��A�"�A�r�A�r�A���A���A�E�A��`A�S�A���A���A��A� �A��A�9XA�r�A�S�A��-A��^A�"�A��A��DA�bA�dZA��wA���A�ZA��DA�r�A}�A{|�Ay�Au�FAu"�At$�Asp�Ap  Am��Ak�Aj1'Ai��Ah�Af�!AdffAc�Ab��Aap�A^�A[�
AZȴAZ=qAX��AV�9AVbAU�AT$�AS�AR1'AP��AO
=ANr�ANbAMAMVAL-AK�PAJ��AI�AI
=AHVAG�wAE��AE/AC�TAB�ABE�AA+A?�FA>��A>�A=A=XA=33A<^5A;p�A:�A9��A8-A7\)A7A533A4�\A3t�A2�/A2-A1XA1&�A0�RA/A.��A.�jA.�A-��A-+A,��A,�!A,$�A*�A*=qA)��A)33A(Q�A'��A&�A&r�A%�;A$��A#�#A#\)A"�DA"1A!��A �jA �uA ��A -AA�`A��A�yA�A�
A�A��A��A%A�uA$�AS�A�A��A��AA�A
=A=qA|�A1'AA�A��AoAA�A
=A
Q�A	K�A�\AbA�;A�hA^5A�-A�A1A
=A�A�FA+A Q�A   @�\)@�-@�p�@��w@��#@��/@��@��#@�D@�b@���@�Ĝ@�33@�!@�V@�p�@�D@��@�S�@�-@�&�@�r�@���@�K�@柾@�-@�/@�9X@��y@�7@ߥ�@�V@݉7@ڸR@�x�@ج@��@��@���@�1@�C�@�$�@�Z@�;d@��#@���@�j@�+@��@ɺ^@�%@Ǿw@��H@ũ�@ļj@+@��@�A�@��@�33@�~�@�J@��-@�`B@��/@��D@��@�^5@���@�x�@��@�33@�ff@�x�@���@��9@��@���@��!@�5?@�p�@��9@��w@�E�@��^@���@�b@��@�33@�-@���@�Z@�1'@���@���@��@��@�x�@���@�bN@� �@��F@�|�@��@�^5@���@�C�@�ȴ@��+@�n�@�=q@���@�G�@�V@�%@���@��`@��`@��@��`@��@��
@��@�;d@���@��@�p�@�O�@��/@�ƨ@���@�t�@�33@�v�@��@���@�?}@�V@�%@���@��`@��j@��D@�A�@� �@��w@��H@���@���@��+@�-@�@�?}@�Ĝ@�9X@��w@�;d@��@���@�-@��@���@���@��7@�hs@�?}@�V@��/@�Ĝ@��j@��j@��D@��w@�K�@��@��@��-@�p�@�`B@�/@��/@��j@���@���@��D@�r�@�Q�@��@��F@���@�+@�~�@�^5@���@���@��^@���@�hs@�7L@��@���@���@�bN@��@��@��@��y@�ȴ@��!@��+@�v�@�n�@�^5@�=q@�@��@��^@���@��h@�x�@�`B@��@���@���@���@���@���@��j@�  @�@~V@}�h@}?}@}/@}V@|z�@{o@z~�@zn�@zM�@yx�@w�@v��@v��@v�+@vE�@v@u`B@uV@t�D@t9X@s�m@sƨ@s��@sdZ@s@r��@r��@r��@r�H@r~�@r-@q��@qG�@p��@p�@oK�@nff@n$�@m�-@m?}@lZ@kS�@j��@jJ@i�#@i�^@i��@ix�@iX@iG�@i&�@h��@g�@g�P@g
=@f�+@fV@f$�@e�-@d��@dI�@d1@c�m@c�m@c��@b�H@b�\@a�@a��@aG�@`�@`bN@`A�@`  @_��@_|�@_l�@_;d@_+@_
=@^�@^�R@^v�@^{@]��@]p�@]�@\�@\�@\j@\9X@[�
@Z�@Z�\@Z^5@Z^5@Z=q@Z�@Y��@Yhs@Y7L@Y&�@Y&�@Y&�@Y�@Y�@Y%@XĜ@X�u@XA�@W�;@W\)@V��@V��@Vff@V5?@U�@U�T@U@U�@T��@T9X@T1@S�m@St�@SC�@R�H@R^5@RM�@R=q@Q��@Q��@Qx�@Q%@P��@PbN@P  @O|�@O
=@N�R@N��@N�+@Nff@N5?@N@M@Mp�@M/@L��@L�@L�D@LZ@L1@K��@KdZ@KC�@J�@J�@I�^@I�^@I��@I��@I7L@HĜ@Hr�@HbN@HQ�@H1'@H �@H  @G�@F�y@F5?@E@E�@Ep�@E`B@E?}@E/@E/@D�/@Dz�@DI�@D9X@D1@C��@C�@Cƨ@Cƨ@CdZ@C@B�H@B��@B=q@B-@B-@B�@BJ@A��@A�#@A�^@AG�@@��@@�`@@Ĝ@@�9@@��@@�u@@�@@�@@r�@@A�@?�w@?|�@?
=@>ȴ@>�R@>v�@>@=��@=�h@=/@=�@=V@=V@<��@<�D@<z�@<j@;��@;ƨ@;��@;C�@:�H@:�!@:�!@:�\@:=q@:J@9�^@9G�@9�@9%@8��@8Ĝ@8�9@8��@8�u@8bN@81'@7�@7|�@7|�@7\)@7
=@6ȴ@6��@6ff@6V@6E�@65?@6{@5��@5p�@5V@4��@4�j@4��@4�D@4�D@4I�@4�@3��@3�F@3��@3t�@3S�@3o@2��@2n�@2M�@2J@1�#@1��@1��@0��@0Ĝ@0��@0bN@01'@0  @/�w@/\)@/+@.�@.�+@.5?@.@-��@-p�@-�@,��@,�@,z�@,I�@+�m@+ƨ@+�F@+��@+dZ@+33@*��@*�\@*M�@)��@)�^@)x�@)%@(�`@(Ĝ@(r�@(A�@( �@( �@(  @'�;@'��@'�P@'K�@&��@&�R@&��@&�+@&v�@&ff@&$�@%@%�@%O�@%/@%V@$�@$�j@$��@$z�@$j@$�@#�F@#"�@"�H@"��@"�\@"~�@"^5@!�@!��@!�7@!x�@!X@!%@ ��@ ��@ Q�@ A�@  �@   @��@�P@\)@K�@
=@�@�@�@��@E�@�@/@�@�D@Z@I�@(�@�F@t�@dZ@dZ@�@��@�!@��@~�@^5@M�@M�@M�@=q@M�@J@�@�^@x�@%@�`@�`@��@�9@�@Q�@A�@ �@b@b@  @�@�;@��@�@��@l�@K�@;d@�y@��@V@�T@@@��@�T@��@�-@�h@�/@z�@1@�F@t�@S�@33@"�@o@�@��@^5@�@J@��@�@x�@G�@%@��@Ĝ@��@�@r�@bN@1'@�;@�P@K�@�@
=@�R@{@�-@�@O�@��@��@��@��@�@z�@j@j@Z@�m@ƨ@ƨ@ƨ@�F@�F@�F@��@��@��@��@dZ@C�@o@
��@
��@
��@
�\@
n�@
^5@
M�@
-@
J@	�@	��@	��@	��@	x�@	X@	G�@	7L@	&�@��@bN@A�@�@��@�@��@|�@\)@K�@�@�y@ff@5?@@�T@�-@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�%A�bA��A��A��A��A��A��A��A��A��A��A��A��A� �A�$�A�$�A�&�A�&�A�&�A�-A�/A�-A� �A��A��A�JA�{AļjAčPA�~�A���A��A�t�A���A�1A��^A��/A�bA��wA�
=A�JA���A��\A�A���A���A�;dA�~�A���A�&�A���A��A�ȴA��A��9A�5?A��#A�G�A��DA��yA���A��A�"�A�r�A�r�A���A���A�E�A��`A�S�A���A���A��A� �A��A�9XA�r�A�S�A��-A��^A�"�A��A��DA�bA�dZA��wA���A�ZA��DA�r�A}�A{|�Ay�Au�FAu"�At$�Asp�Ap  Am��Ak�Aj1'Ai��Ah�Af�!AdffAc�Ab��Aap�A^�A[�
AZȴAZ=qAX��AV�9AVbAU�AT$�AS�AR1'AP��AO
=ANr�ANbAMAMVAL-AK�PAJ��AI�AI
=AHVAG�wAE��AE/AC�TAB�ABE�AA+A?�FA>��A>�A=A=XA=33A<^5A;p�A:�A9��A8-A7\)A7A533A4�\A3t�A2�/A2-A1XA1&�A0�RA/A.��A.�jA.�A-��A-+A,��A,�!A,$�A*�A*=qA)��A)33A(Q�A'��A&�A&r�A%�;A$��A#�#A#\)A"�DA"1A!��A �jA �uA ��A -AA�`A��A�yA�A�
A�A��A��A%A�uA$�AS�A�A��A��AA�A
=A=qA|�A1'AA�A��AoAA�A
=A
Q�A	K�A�\AbA�;A�hA^5A�-A�A1A
=A�A�FA+A Q�A   @�\)@�-@�p�@��w@��#@��/@��@��#@�D@�b@���@�Ĝ@�33@�!@�V@�p�@�D@��@�S�@�-@�&�@�r�@���@�K�@柾@�-@�/@�9X@��y@�7@ߥ�@�V@݉7@ڸR@�x�@ج@��@��@���@�1@�C�@�$�@�Z@�;d@��#@���@�j@�+@��@ɺ^@�%@Ǿw@��H@ũ�@ļj@+@��@�A�@��@�33@�~�@�J@��-@�`B@��/@��D@��@�^5@���@�x�@��@�33@�ff@�x�@���@��9@��@���@��!@�5?@�p�@��9@��w@�E�@��^@���@�b@��@�33@�-@���@�Z@�1'@���@���@��@��@�x�@���@�bN@� �@��F@�|�@��@�^5@���@�C�@�ȴ@��+@�n�@�=q@���@�G�@�V@�%@���@��`@��`@��@��`@��@��
@��@�;d@���@��@�p�@�O�@��/@�ƨ@���@�t�@�33@�v�@��@���@�?}@�V@�%@���@��`@��j@��D@�A�@� �@��w@��H@���@���@��+@�-@�@�?}@�Ĝ@�9X@��w@�;d@��@���@�-@��@���@���@��7@�hs@�?}@�V@��/@�Ĝ@��j@��j@��D@��w@�K�@��@��@��-@�p�@�`B@�/@��/@��j@���@���@��D@�r�@�Q�@��@��F@���@�+@�~�@�^5@���@���@��^@���@�hs@�7L@��@���@���@�bN@��@��@��@��y@�ȴ@��!@��+@�v�@�n�@�^5@�=q@�@��@��^@���@��h@�x�@�`B@��@���@���@���@���@���@��j@�  @�@~V@}�h@}?}@}/@}V@|z�@{o@z~�@zn�@zM�@yx�@w�@v��@v��@v�+@vE�@v@u`B@uV@t�D@t9X@s�m@sƨ@s��@sdZ@s@r��@r��@r��@r�H@r~�@r-@q��@qG�@p��@p�@oK�@nff@n$�@m�-@m?}@lZ@kS�@j��@jJ@i�#@i�^@i��@ix�@iX@iG�@i&�@h��@g�@g�P@g
=@f�+@fV@f$�@e�-@d��@dI�@d1@c�m@c�m@c��@b�H@b�\@a�@a��@aG�@`�@`bN@`A�@`  @_��@_|�@_l�@_;d@_+@_
=@^�@^�R@^v�@^{@]��@]p�@]�@\�@\�@\j@\9X@[�
@Z�@Z�\@Z^5@Z^5@Z=q@Z�@Y��@Yhs@Y7L@Y&�@Y&�@Y&�@Y�@Y�@Y%@XĜ@X�u@XA�@W�;@W\)@V��@V��@Vff@V5?@U�@U�T@U@U�@T��@T9X@T1@S�m@St�@SC�@R�H@R^5@RM�@R=q@Q��@Q��@Qx�@Q%@P��@PbN@P  @O|�@O
=@N�R@N��@N�+@Nff@N5?@N@M@Mp�@M/@L��@L�@L�D@LZ@L1@K��@KdZ@KC�@J�@J�@I�^@I�^@I��@I��@I7L@HĜ@Hr�@HbN@HQ�@H1'@H �@H  @G�@F�y@F5?@E@E�@Ep�@E`B@E?}@E/@E/@D�/@Dz�@DI�@D9X@D1@C��@C�@Cƨ@Cƨ@CdZ@C@B�H@B��@B=q@B-@B-@B�@BJ@A��@A�#@A�^@AG�@@��@@�`@@Ĝ@@�9@@��@@�u@@�@@�@@r�@@A�@?�w@?|�@?
=@>ȴ@>�R@>v�@>@=��@=�h@=/@=�@=V@=V@<��@<�D@<z�@<j@;��@;ƨ@;��@;C�@:�H@:�!@:�!@:�\@:=q@:J@9�^@9G�@9�@9%@8��@8Ĝ@8�9@8��@8�u@8bN@81'@7�@7|�@7|�@7\)@7
=@6ȴ@6��@6ff@6V@6E�@65?@6{@5��@5p�@5V@4��@4�j@4��@4�D@4�D@4I�@4�@3��@3�F@3��@3t�@3S�@3o@2��@2n�@2M�@2J@1�#@1��@1��@0��@0Ĝ@0��@0bN@01'@0  @/�w@/\)@/+@.�@.�+@.5?@.@-��@-p�@-�@,��@,�@,z�@,I�@+�m@+ƨ@+�F@+��@+dZ@+33@*��@*�\@*M�@)��@)�^@)x�@)%@(�`@(Ĝ@(r�@(A�@( �@( �@(  @'�;@'��@'�P@'K�@&��@&�R@&��@&�+@&v�@&ff@&$�@%@%�@%O�@%/@%V@$�@$�j@$��@$z�@$j@$�@#�F@#"�@"�H@"��@"�\@"~�@"^5@!�@!��@!�7@!x�@!X@!%@ ��@ ��@ Q�@ A�@  �@   @��@�P@\)@K�@
=@�@�@�@��@E�@�@/@�@�D@Z@I�@(�@�F@t�@dZ@dZ@�@��@�!@��@~�@^5@M�@M�@M�@=q@M�@J@�@�^@x�@%@�`@�`@��@�9@�@Q�@A�@ �@b@b@  @�@�;@��@�@��@l�@K�@;d@�y@��@V@�T@@@��@�T@��@�-@�h@�/@z�@1@�F@t�@S�@33@"�@o@�@��@^5@�@J@��@�@x�@G�@%@��@Ĝ@��@�@r�@bN@1'@�;@�P@K�@�@
=@�R@{@�-@�@O�@��@��@��@��@�@z�@j@j@Z@�m@ƨ@ƨ@ƨ@�F@�F@�F@��@��@��@��@dZ@C�@o@
��@
��@
��@
�\@
n�@
^5@
M�@
-@
J@	�@	��@	��@	��@	x�@	X@	G�@	7L@	&�@��@bN@A�@�@��@�@��@|�@\)@K�@�@�y@ff@5?@@�T@�-@��@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�mB�ZB�)B�}B]/BhB+B��B�B�yB�mB�B�TB�#B��B��B��B��BǮBĜBĜB��B�jB�LB�!B��B��B�bB�=B�Bv�BcTBM�BC�B5?B+B�B\BB��B�B�B�B��BĜB��B��B�7B}�Bo�BcTBW
BK�B@�B#�BJB�B�B�fB�HB��BĜB�B��B�oBv�Bq�Bk�BffBXBJ�B7LB-B)�B!�BuB+BB��B��B�yB�)B�B��B��BȴBÖBB�wB�dB�RB�'B��B��B��B��B��B��B��B��B��B��B�oB�bB�DB�7B�+B�B� B{�Bv�Bt�Bs�Bq�Bo�Bn�Bl�BgmBcTB`BB]/BZB]/BVBW
BR�BP�BP�BP�BO�BN�BK�BG�BH�BG�BE�BC�BB�BA�B@�B<jB<jB;dB8RB49B2-B/B-B)�B&�B!�B�B�B�B�B�B�B�B�B�B�BhBJBDB
=B+B+BBBB  B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�sB
�fB
�`B
�TB
�NB
�HB
�HB
�;B
�;B
�/B
�/B
�B
�B
�B
�B
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
ɺB
ȴB
ɺB
ŢB
ŢB
ÖB
ĜB
B
��B
��B
��B
��B
��B
��B
�}B
�}B
�wB
�wB
�wB
�qB
�jB
�jB
�dB
�dB
�^B
�^B
�XB
�RB
�XB
�RB
�RB
�XB
�RB
�XB
�XB
�XB
�^B
�XB
�^B
�dB
�XB
�LB
�XB
�^B
�^B
�wB
��B
��B
�}B
�}B
B
ĜB
ƨB
ƨB
ǮB
ɺB
ɺB
��B
��B
��B
��B
��B
�B
�
B
�B
�B
�)B
�;B
�HB
�NB
�HB
�`B
�`B
�fB
�mB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��BB%B1B
=BDBVB�B�B�B�B�B!�B"�B$�B&�B.B7LB:^B<jB<jB=qB@�BB�BC�BC�BC�BD�BD�BD�BC�BD�BI�BL�BM�BQ�BT�BYBYB[#BbNBbNBcTBe`BjBl�Bn�Bp�Bq�Br�Br�Bs�Bt�Bv�Bx�Bx�B{�B�B�B�B�B�B�%B�=B�PB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�FB�RB�jB�}B��B��BÖBƨBǮBȴBȴBɺB��B��B��B��B��B��B�B�)B�;B�BB�NB�TB�ZB�fB�mB�sB�yB�B�B�B��B��B��B��B��B��B��B  BBBB+B1B	7B
=B
=BJB\BbBhBhBhBhB�B�B#�B&�B'�B(�B(�B+B2-B49B49B49B7LB<jB=qB@�BB�BE�BF�BF�BH�BI�BJ�BK�BJ�BJ�BJ�BK�BM�BN�BO�BR�BS�BS�BVBZB[#B]/B`BBdZBe`BffBffBgmBiyBl�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bq�Bt�Bu�Bw�By�Bz�Bz�B|�B~�B�B�B�B�B�B�1B�7B�DB�JB�PB�bB�bB�hB�hB�uB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�-B�3B�9B�3B�9B�FB�LB�XB�XB�XB�dB�dB�qB�wB�wB�}B�}B��B��BBÖBĜBŢBƨBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�
B�B�B�B�B�B�B�B�#B�/B�5B�;B�;B�;B�;B�BB�HB�NB�NB�TB�TB�TB�TB�TB�TB�`B�mB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  B  B  BBBBBBBBBBBBBBBB%B%B%B+B+B+B+B	7B	7B	7B
=B
=B
=B
=BDBDBJBJBPBPBVBVB\B\B\BbBbBhBhBhBhBhBoBoBoBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B!�B!�B!�B"�B"�B"�B"�B#�B#�B#�B$�B$�B$�B$�B$�B$�B%�B%�B&�B'�B'�B(�B(�B(�B(�B)�B)�B)�B)�B+B+B+B,B,B,B,B,B,B,B,B-B-B-B.B/B/B/B/B/B/B0!B0!B0!B0!B0!B0!B1'B1'B1'B1'B1'B1'B2-B2-B2-B33B33B49B49B49B49B49B49B49B49B5?B6FB7LB7LB7LB8RB8RB8RB8RB8RB9XB9XB9XB:^B9XB:^B:^B;dB;dB;dB;dB<jB<jB<jB<jB<jB=qB=qB=qB>wB>wB>wB?}B@�B@�B@�BA�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BD�BD�BD�BD�BD�BE�BE�BE�BE�BE�BE�BF�BF�BF�BF�BF�BF�BG�BG�BG�BH�BH�BH�BI�BI�BI�BI�BI�BI�BI�BJ�BJ�BK�BK�BK�BK�BL�BL�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�mB�ZB�)B�}B]/BhB+B��B�B�yB�mB�B�TB�#B��B��B��B��BǮBĜBĜB��B�jB�LB�!B��B��B�bB�=B�Bv�BcTBM�BC�B5?B+B�B\BB��B�B�B�B��BĜB��B��B�7B}�Bo�BcTBW
BK�B@�B#�BJB�B�B�fB�HB��BĜB�B��B�oBv�Bq�Bk�BffBXBJ�B7LB-B)�B!�BuB+BB��B��B�yB�)B�B��B��BȴBÖBB�wB�dB�RB�'B��B��B��B��B��B��B��B��B��B��B�oB�bB�DB�7B�+B�B� B{�Bv�Bt�Bs�Bq�Bo�Bn�Bl�BgmBcTB`BB]/BZB]/BVBW
BR�BP�BP�BP�BO�BN�BK�BG�BH�BG�BE�BC�BB�BA�B@�B<jB<jB;dB8RB49B2-B/B-B)�B&�B!�B�B�B�B�B�B�B�B�B�B�BhBJBDB
=B+B+BBBB  B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�sB
�fB
�`B
�TB
�NB
�HB
�HB
�;B
�;B
�/B
�/B
�B
�B
�B
�B
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
ɺB
ȴB
ɺB
ŢB
ŢB
ÖB
ĜB
B
��B
��B
��B
��B
��B
��B
�}B
�}B
�wB
�wB
�wB
�qB
�jB
�jB
�dB
�dB
�^B
�^B
�XB
�RB
�XB
�RB
�RB
�XB
�RB
�XB
�XB
�XB
�^B
�XB
�^B
�dB
�XB
�LB
�XB
�^B
�^B
�wB
��B
��B
�}B
�}B
B
ĜB
ƨB
ƨB
ǮB
ɺB
ɺB
��B
��B
��B
��B
��B
�B
�
B
�B
�B
�)B
�;B
�HB
�NB
�HB
�`B
�`B
�fB
�mB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��BB%B1B
=BDBVB�B�B�B�B�B!�B"�B$�B&�B.B7LB:^B<jB<jB=qB@�BB�BC�BC�BC�BD�BD�BD�BC�BD�BI�BL�BM�BQ�BT�BYBYB[#BbNBbNBcTBe`BjBl�Bn�Bp�Bq�Br�Br�Bs�Bt�Bv�Bx�Bx�B{�B�B�B�B�B�B�%B�=B�PB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�FB�RB�jB�}B��B��BÖBƨBǮBȴBȴBɺB��B��B��B��B��B��B�B�)B�;B�BB�NB�TB�ZB�fB�mB�sB�yB�B�B�B��B��B��B��B��B��B��B  BBBB+B1B	7B
=B
=BJB\BbBhBhBhBhB�B�B#�B&�B'�B(�B(�B+B2-B49B49B49B7LB<jB=qB@�BB�BE�BF�BF�BH�BI�BJ�BK�BJ�BJ�BJ�BK�BM�BN�BO�BR�BS�BS�BVBZB[#B]/B`BBdZBe`BffBffBgmBiyBl�Bn�Bo�Bo�Bo�Bp�Bp�Bp�Bp�Bq�Bt�Bu�Bw�By�Bz�Bz�B|�B~�B�B�B�B�B�B�1B�7B�DB�JB�PB�bB�bB�hB�hB�uB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�-B�3B�9B�3B�9B�FB�LB�XB�XB�XB�dB�dB�qB�wB�wB�}B�}B��B��BBÖBĜBŢBƨBȴBɺBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�
B�B�B�B�B�B�B�B�#B�/B�5B�;B�;B�;B�;B�BB�HB�NB�NB�TB�TB�TB�TB�TB�TB�`B�mB�sB�sB�sB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B  B  B  BBBBBBBBBBBBBBBB%B%B%B+B+B+B+B	7B	7B	7B
=B
=B
=B
=BDBDBJBJBPBPBVBVB\B\B\BbBbBhBhBhBhBhBoBoBoBuBuB{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B �B!�B!�B!�B"�B"�B"�B"�B#�B#�B#�B$�B$�B$�B$�B$�B$�B%�B%�B&�B'�B'�B(�B(�B(�B(�B)�B)�B)�B)�B+B+B+B,B,B,B,B,B,B,B,B-B-B-B.B/B/B/B/B/B/B0!B0!B0!B0!B0!B0!B1'B1'B1'B1'B1'B1'B2-B2-B2-B33B33B49B49B49B49B49B49B49B49B5?B6FB7LB7LB7LB8RB8RB8RB8RB8RB9XB9XB9XB:^B9XB:^B:^B;dB;dB;dB;dB<jB<jB<jB<jB<jB=qB=qB=qB>wB>wB>wB?}B@�B@�B@�BA�BA�BA�BA�BA�BA�BB�BB�BB�BB�BB�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BC�BD�BD�BD�BD�BD�BE�BE�BE�BE�BE�BE�BF�BF�BF�BF�BF�BF�BG�BG�BG�BH�BH�BH�BI�BI�BI�BI�BI�BI�BI�BJ�BJ�BK�BK�BK�BK�BL�BL�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.19 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20211115100105                              AO  ARCAADJP                                                                    20211115100105    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20211115100105  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20211115100105  QCF$                G�O�G�O�G�O�8000            