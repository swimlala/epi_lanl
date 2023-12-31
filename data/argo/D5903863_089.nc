CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       d2014-07-21T23:39:42Z creation; 2014-07-21T23:39:42Z updated; 2015-09-28T12:13:13Z converted from 3.0   
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7    PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7`   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8    PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8$   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8D   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8d   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           8h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8p   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            8t   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8|   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        l  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \\   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  `8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  o�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �     TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �4   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ۜ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20140721233942  20170523133346  5903863 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               YA   AO  4298_0127_089                   2C  D   NAVIS_A                         0127                            120111                          863 @��a���1   @��bPg?�@6*��n��co�O�;d1   GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                      YA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO�fDP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�ff@�ffA33A733AW33Aw33A���A���A���A���A˙�Aۙ�A뙚A���B��B��B��B��B%��B-��B5��B=��BE��BM��BU��B]��Be��Bm��Bu��B}��B��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fB��fCs3Cs3Cs3Cs3C	s3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3Cs3C!s3C#s3C%s3C's3C)s3C+s3C-s3C/s3C1s3C3s3C5s3C7s3C9s3C;s3C=s3C?s3CAs3CCs3CEs3CGs3CIs3CKs3CMs3COs3CQs3CSs3CUs3CWs3CYs3C[s3C]s3C_s3Cas3Ccs3Ces3Cgs3Cis3Cks3Cms3Cos3Cqs3Css3Cus3Cws3Cys3C{s3C}s3Cs3C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��gC���C¹�Cù�CĹ�CŹ�Cƹ�Cǹ�Cȹ�Cɹ�Cʹ�C˹�C̹�C͹�Cι�CϹ�Cй�Cѹ�Cҹ�Cӹ�CԹ�Cչ�Cֹ�C׹�Cع�Cٹ�Cڹ�C۹�Cܹ�Cݹ�C޹�C߹�C๚CṚC⹚C㹚C乚C幚C湚C繚C蹚C鹚C깚C빚C칚C���CC﹚C�C�C�C�C���C���C���C���C���C���C���C���C���C���C���C���D \�D ��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D	\�D	��D
\�D
��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D\�D��D \�D ��D!\�D!��D"\�D"��D#\�D#��D$\�D$��D%\�D%��D&\�D&��D'\�D'��D(\�D(��D)\�D)��D*\�D*��D+\�D+��D,\�D,��D-\�D-��D.\�D.��D/\�D/��D0\�D0��D1\�D1��D2\�D2��D3\�D3��D4\�D4��D5\�D5��D6\�D6��D7\�D7��D8\�D8��D9\�D9��D:\�D:��D;\�D;��D<\�D<��D=\�D=��D>\�D>��D?\�D?��D@\�D@��DA\�DA��DB\�DB��DC\�DC��DD\�DD��DE\�DE��DF\�DF��DG\�DG��DH\�DH��DI\�DI��DJ\�DJ��DK\�DK��DL\�DL��DM\�DM��DN\�DN��DOc3DO��DP\�DP��DQ\�DQ��DR\�DR��DS\�DS��DT\�DT��DU\�DU��DV\�DV��DW\�DW��DX\�DX��DY\�DY��DZ\�DZ��D[\�D[��D\\�D\��D]\�D]��D^\�D^��D_\�D_��D`\�D`��Da\�Da��Db\�Db��Dc\�Dc��Dd\�Dd��De\�De��Df\�Df��Dg\�Dg��Dh\�Dh��Di\�Di��Dj\�Dj��Dk\�Dk��Dl\�Dl��Dmc3Dm��Dn\�Dn��Do\�Do��Dp\�Dp��Dq\�Dq��Dr\�Dr��Ds\�Ds��Dt\�Dt��Du\�Du��Dv\�Dv��Dw\�Dw��Dx\�Dx��Dy\�Dy��Dz\�Dz��D{\�D{��D|\�D|��D}\�D}��D~\�D~��D\�D��D�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��3D�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD®fD��fD�.fD�nfDîfD��fD�.fD�nfDĮfD��fD�.fD�nfDŮfD��fD�.fD�nfDƮfD��fD�.fD�nfDǮfD��fD�.fD�nfDȮfD��fD�.fD�nfDɮfD��fD�.fD�nfDʮfD��fD�.fD�nfDˮfD��fD�.fD�nfD̮fD��fD�.fD�nfDͮfD��fD�.fD�nfDήfD��fD�.fD�nfDϮfD��fD�.fD�nfDЮfD��fD�.fD�nfDѮfD��fD�.fD�nfDҮfD��fD�.fD�nfDӮfD��fD�.fD�nfDԮfD��fD�.fD�nfDծfD��fD�.fD�nfD֮fD��fD�.fD�nfD׮fD��fD�.fD�nfDخfD��fD�.fD�nfDٮfD��fD�.fD�nfDڮfD��fD�.fD�nfDۮfD��fD�.fD�nfDܮfD��fD�.fD�nfDݮfD��fD�.fD�nfDޮfD��fD�.fD�nfD߮fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD��fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD�fD��fD�.fD�nfD��fD��fD�.fD�nfD��fD��fD�.fD�nfD���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�VA�p�A�r�A�l�A�p�A�v�A�r�A�K�A�9XA��A�{A�oA�1A��A��/A�ƨA˼jA˧�AˁA�{AʬA�|�A�n�A�%AɃA��A�+A��yA�~�A£�A®A¬A�VA��
A�Q�A��A�ZA��jA�-A��FA�oA���A���A��A���A��A��hA��^A�(�A�ĜA���A���A�bA���A�r�A�A��A�-A���A���A�+A�oA��A�(�A��A�dZA��A�;dA�A��A�x�A��/A�v�A���A��hA�1A���A�\)A�9XA�O�A�v�A���A�ffA��A���A��PA��hA�G�A�ZA��A���A��
A�~�A�r�A���A���A��/A�ȴA��RA��A|9XAz�jAx  At�\Ap�Am�hAl�Ak�Aj�!Ai�Af��Af�Ae33Ac?}A`�A]�wAXbNATJAPM�AN�\AM�wAM��AM`BAL��AJ�AI�
AI/AG��AE��AD��AD{AA33A?;dA=��A=oA<��A<�uA:�A:1A9��A9��A9+A8��A7+A6VA61A5XA4��A4�uA4$�A3�-A2�jA1�PA/�A.bA-�A+��A*�DA*9XA)|�A(jA'�A'�A&v�A%/A$�+A$~�A#\)A!��A ��A��A^5A�A�A�+AE�A �A{A�yA\)A�A��A��AE�Ax�AffA�Ax�A��AC�A
M�AĜA  AQ�AAr�A��AȴA=qAC�@��w@�`B@���@�x�@���@��w@�C�@�"�@���@���@�Ĝ@� �@��m@�ƨ@�dZ@�v�@�%@��D@��@�~�@�@ꗍ@�I�@��@��#@�7L@�Ĝ@� �@���@���@��`@�|�@ܛ�@��@�G�@���@��@��;@љ�@��
@��@̬@�dZ@���@ɡ�@� �@�S�@�;d@�+@�@Ɨ�@�V@š�@���@Õ�@§�@��#@�/@�9X@�;d@���@��#@���@��7@�hs@��@�bN@���@�K�@���@���@��+@�$�@�@�p�@�O�@�7L@���@��D@�I�@���@���@��@�@��y@�;d@���@�ȴ@��7@�V@�9X@���@�;d@��\@��@�`B@���@��
@��P@�l�@�K�@��y@��\@�V@��#@�V@��@�Ĝ@�1'@��@�|�@��@���@��w@�S�@�K�@��;@�\)@���@�V@�^5@�-@��@��7@�`B@�&�@�V@�r�@�1'@��m@��F@���@���@��@�S�@�"�@���@�=q@���@�ȴ@���@��@��@��H@��@�ff@��T@��7@��@���@��j@���@���@�j@�b@��m@�|�@�^5@��7@�p�@�G�@�V@�Ĝ@��@�  @�33@���@�V@�J@��T@��T@��#@�&�@��j@���@�  @���@�o@���@��y@���@�=q@��-@�x�@�x�@�`B@���@���@��j@�r�@�|�@��!@�@��^@�O�@���@�p�@��j@���@���@���@��@�9X@��@��@��@�t�@�S�@�K�@��@�o@��y@��R@�-@�?}@��D@��9@��F@��w@�l�@�l�@��F@�|�@�
=@���@�?}@���@�j@�  @��F@�dZ@�K�@�"�@��H@���@���@��+@�M�@��@��@�hs@�G�@�%@���@�Q�@�9X@�9X@��m@���@�\)@�
=@��y@���@���@�M�@��^@��h@�x�@�?}@��@���@���@�(�@�Q�@��u@��@�Q�@���@���@���@�\)@���@�v�@�n�@�ff@�^5@�5?@�O�@���@��@���@���@��u@�I�@��@�@~5?@}��@}`B@|�/@|Z@|1@{dZ@z�H@z��@z��@z~�@zJ@y��@y��@y��@yx�@x��@w�w@vȴ@v�+@vV@v{@u��@uO�@t�j@tz�@t�D@tZ@t(�@sƨ@s"�@r��@rn�@r^5@rM�@r=q@r-@q��@q��@qhs@qhs@qG�@q�@p��@p��@p�@pr�@pbN@p��@p��@pr�@pr�@pA�@pb@o�w@o;d@n�@n��@n��@n$�@mp�@mV@lj@k��@j�H@j�\@jJ@i�@iG�@hĜ@h1'@g�@g�@g;d@f��@fv�@f{@e`B@d�@dj@c�F@cdZ@b�H@bn�@b�@a��@a��@a��@a�7@aX@a7L@ahs@a&�@`��@`��@`A�@_�@_�@^�R@^��@^$�@]O�@]�@\�j@\9X@[t�@[33@Z��@Z~�@Z^5@Z�@Y�^@Y�7@YX@Y�@X�`@X�`@X�9@X �@W��@W�w@W�P@W\)@W;d@W+@W
=@V�y@Vȴ@Vȴ@Vȴ@V�R@V��@V5?@U@Up�@T�@T�j@T�j@T�@T�@T�@T�@T�@Tz�@T9X@Sƨ@SdZ@SC�@S33@S33@S33@S"�@S@R�@R�@R��@RM�@Q��@QX@P��@P�`@P��@Pr�@PbN@P �@O�@O
=@N��@Nff@NE�@N{@M@MO�@MV@L�j@Lz�@LI�@L�@L1@K�m@K�
@KdZ@K33@J�H@J=q@I��@IG�@I&�@H��@HbN@HA�@H  @G�@G�@G�@G�;@G�w@G|�@Gl�@G
=@F$�@E�@E��@Ep�@EO�@D�@D��@D�j@D�@D�D@D(�@C��@Ct�@Ct�@CC�@B��@Bn�@B^5@Ax�@@��@?�@?\)@>��@=�-@=?}@=�@<��@<��@<9X@;��@;dZ@:n�@9��@8Ĝ@7��@6��@6��@6ff@5�@5V@4�j@4Z@3��@2��@2=q@1��@1G�@1%@0��@0��@0�u@0r�@0 �@/�w@/l�@.�y@.��@.�+@.ff@.V@.E�@.5?@.{@-�T@-@-�-@-�h@-�h@-p�@-V@,��@,��@,z�@,1@+�m@+ƨ@+t�@+dZ@+S�@+"�@+@*�!@*M�@)�^@)7L@)�@)&�@)&�@)&�@)�@)�@)%@)%@(�`@(��@(�@(bN@(Q�@(Q�@(A�@(1'@( �@'�@'�P@'K�@';d@'+@'+@'�@&�@&ȴ@&��@&V@&{@%�@%�h@%O�@%�@$�@$��@$z�@$Z@$9X@#��@#ƨ@#��@#��@#��@#��@#��@#�@#t�@#C�@#dZ@#S�@#33@"��@"��@"~�@"M�@"J@!�#@!G�@ Ĝ@ A�@   @�;@��@�P@��@E�@�T@�@O�@?}@?}@?}@��@�@z�@9X@��@dZ@S�@C�@"�@@n�@J@�#@�^@��@��@hs@�@%@��@��@��@bN@�;@|�@;d@;d@�@�@��@ff@@��@�@?}@��@�@�@��@�j@�@�@�@��@��@��@�D@j@9X@�
@��@dZ@C�@@��@��@��@�!@��@�\@-@��@�#@�^@��@�7@x�@hs@X@G�@�@�`@Ĝ@��@�u@�u@�@r�@bN@bN@Q�@1'@ �@�;@�P@l�@+@
=@��@ȴ@��@��@V@�T@��@O�@?}@V@�j@��@z�@I�@9X@(�@��@��@�@o@@@
�@
�H@
�!@
�\@
~�@
^5@
M�@
�@	��@	��@	7L@	&�@	&�@	7L@	7L@	7L@	&�@	&�@	&�@��@��@��@��@Ĝ@Ĝ@Ĝ@�9@�9@�u@r�@b@�w@|�@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�VA�p�A�r�A�l�A�p�A�v�A�r�A�K�A�9XA��A�{A�oA�1A��A��/A�ƨA˼jA˧�AˁA�{AʬA�|�A�n�A�%AɃA��A�+A��yA�~�A£�A®A¬A�VA��
A�Q�A��A�ZA��jA�-A��FA�oA���A���A��A���A��A��hA��^A�(�A�ĜA���A���A�bA���A�r�A�A��A�-A���A���A�+A�oA��A�(�A��A�dZA��A�;dA�A��A�x�A��/A�v�A���A��hA�1A���A�\)A�9XA�O�A�v�A���A�ffA��A���A��PA��hA�G�A�ZA��A���A��
A�~�A�r�A���A���A��/A�ȴA��RA��A|9XAz�jAx  At�\Ap�Am�hAl�Ak�Aj�!Ai�Af��Af�Ae33Ac?}A`�A]�wAXbNATJAPM�AN�\AM�wAM��AM`BAL��AJ�AI�
AI/AG��AE��AD��AD{AA33A?;dA=��A=oA<��A<�uA:�A:1A9��A9��A9+A8��A7+A6VA61A5XA4��A4�uA4$�A3�-A2�jA1�PA/�A.bA-�A+��A*�DA*9XA)|�A(jA'�A'�A&v�A%/A$�+A$~�A#\)A!��A ��A��A^5A�A�A�+AE�A �A{A�yA\)A�A��A��AE�Ax�AffA�Ax�A��AC�A
M�AĜA  AQ�AAr�A��AȴA=qAC�@��w@�`B@���@�x�@���@��w@�C�@�"�@���@���@�Ĝ@� �@��m@�ƨ@�dZ@�v�@�%@��D@��@�~�@�@ꗍ@�I�@��@��#@�7L@�Ĝ@� �@���@���@��`@�|�@ܛ�@��@�G�@���@��@��;@љ�@��
@��@̬@�dZ@���@ɡ�@� �@�S�@�;d@�+@�@Ɨ�@�V@š�@���@Õ�@§�@��#@�/@�9X@�;d@���@��#@���@��7@�hs@��@�bN@���@�K�@���@���@��+@�$�@�@�p�@�O�@�7L@���@��D@�I�@���@���@��@�@��y@�;d@���@�ȴ@��7@�V@�9X@���@�;d@��\@��@�`B@���@��
@��P@�l�@�K�@��y@��\@�V@��#@�V@��@�Ĝ@�1'@��@�|�@��@���@��w@�S�@�K�@��;@�\)@���@�V@�^5@�-@��@��7@�`B@�&�@�V@�r�@�1'@��m@��F@���@���@��@�S�@�"�@���@�=q@���@�ȴ@���@��@��@��H@��@�ff@��T@��7@��@���@��j@���@���@�j@�b@��m@�|�@�^5@��7@�p�@�G�@�V@�Ĝ@��@�  @�33@���@�V@�J@��T@��T@��#@�&�@��j@���@�  @���@�o@���@��y@���@�=q@��-@�x�@�x�@�`B@���@���@��j@�r�@�|�@��!@�@��^@�O�@���@�p�@��j@���@���@���@��@�9X@��@��@��@�t�@�S�@�K�@��@�o@��y@��R@�-@�?}@��D@��9@��F@��w@�l�@�l�@��F@�|�@�
=@���@�?}@���@�j@�  @��F@�dZ@�K�@�"�@��H@���@���@��+@�M�@��@��@�hs@�G�@�%@���@�Q�@�9X@�9X@��m@���@�\)@�
=@��y@���@���@�M�@��^@��h@�x�@�?}@��@���@���@�(�@�Q�@��u@��@�Q�@���@���@���@�\)@���@�v�@�n�@�ff@�^5@�5?@�O�@���@��@���@���@��u@�I�@��@�@~5?@}��@}`B@|�/@|Z@|1@{dZ@z�H@z��@z��@z~�@zJ@y��@y��@y��@yx�@x��@w�w@vȴ@v�+@vV@v{@u��@uO�@t�j@tz�@t�D@tZ@t(�@sƨ@s"�@r��@rn�@r^5@rM�@r=q@r-@q��@q��@qhs@qhs@qG�@q�@p��@p��@p�@pr�@pbN@p��@p��@pr�@pr�@pA�@pb@o�w@o;d@n�@n��@n��@n$�@mp�@mV@lj@k��@j�H@j�\@jJ@i�@iG�@hĜ@h1'@g�@g�@g;d@f��@fv�@f{@e`B@d�@dj@c�F@cdZ@b�H@bn�@b�@a��@a��@a��@a�7@aX@a7L@ahs@a&�@`��@`��@`A�@_�@_�@^�R@^��@^$�@]O�@]�@\�j@\9X@[t�@[33@Z��@Z~�@Z^5@Z�@Y�^@Y�7@YX@Y�@X�`@X�`@X�9@X �@W��@W�w@W�P@W\)@W;d@W+@W
=@V�y@Vȴ@Vȴ@Vȴ@V�R@V��@V5?@U@Up�@T�@T�j@T�j@T�@T�@T�@T�@T�@Tz�@T9X@Sƨ@SdZ@SC�@S33@S33@S33@S"�@S@R�@R�@R��@RM�@Q��@QX@P��@P�`@P��@Pr�@PbN@P �@O�@O
=@N��@Nff@NE�@N{@M@MO�@MV@L�j@Lz�@LI�@L�@L1@K�m@K�
@KdZ@K33@J�H@J=q@I��@IG�@I&�@H��@HbN@HA�@H  @G�@G�@G�@G�;@G�w@G|�@Gl�@G
=@F$�@E�@E��@Ep�@EO�@D�@D��@D�j@D�@D�D@D(�@C��@Ct�@Ct�@CC�@B��@Bn�@B^5@Ax�@@��@?�@?\)@>��@=�-@=?}@=�@<��@<��@<9X@;��@;dZ@:n�@9��@8Ĝ@7��@6��@6��@6ff@5�@5V@4�j@4Z@3��@2��@2=q@1��@1G�@1%@0��@0��@0�u@0r�@0 �@/�w@/l�@.�y@.��@.�+@.ff@.V@.E�@.5?@.{@-�T@-@-�-@-�h@-�h@-p�@-V@,��@,��@,z�@,1@+�m@+ƨ@+t�@+dZ@+S�@+"�@+@*�!@*M�@)�^@)7L@)�@)&�@)&�@)&�@)�@)�@)%@)%@(�`@(��@(�@(bN@(Q�@(Q�@(A�@(1'@( �@'�@'�P@'K�@';d@'+@'+@'�@&�@&ȴ@&��@&V@&{@%�@%�h@%O�@%�@$�@$��@$z�@$Z@$9X@#��@#ƨ@#��@#��@#��@#��@#��@#�@#t�@#C�@#dZ@#S�@#33@"��@"��@"~�@"M�@"J@!�#@!G�@ Ĝ@ A�@   @�;@��@�P@��@E�@�T@�@O�@?}@?}@?}@��@�@z�@9X@��@dZ@S�@C�@"�@@n�@J@�#@�^@��@��@hs@�@%@��@��@��@bN@�;@|�@;d@;d@�@�@��@ff@@��@�@?}@��@�@�@��@�j@�@�@�@��@��@��@�D@j@9X@�
@��@dZ@C�@@��@��@��@�!@��@�\@-@��@�#@�^@��@�7@x�@hs@X@G�@�@�`@Ĝ@��@�u@�u@�@r�@bN@bN@Q�@1'@ �@�;@�P@l�@+@
=@��@ȴ@��@��@V@�T@��@O�@?}@V@�j@��@z�@I�@9X@(�@��@��@�@o@@@
�@
�H@
�!@
�\@
~�@
^5@
M�@
�@	��@	��@	7L@	&�@	&�@	7L@	7L@	7L@	&�@	&�@	&�@��@��@��@��@Ĝ@Ĝ@Ĝ@�9@�9@�u@r�@b@�w@|�@;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B�B��B��B��B1B!�B$�B#�B#�B$�B&�B%�B$�B$�B)�B5?BL�B~�B�B�FB��BŢB�wB�FB�{B�B�DB��B��B��B�B�B��B� BhsB_;BaHBe`Bt�B�%B�hB�=B�oB�+B��B��B��B�bB�Bt�BiyBT�B?}B,B�BPB��B��BȴB�RB�B�B�B�'B��B��B��B�^B�9B�B��BȴB�qB�3B�3B�B��B�+Bs�BM�B8RB49B;dBE�B5?B�BPB
��B
�TB
�;B
�
B
ǮB
�RB
��B
�B
^5B
E�B
)�B
+B	��B	�/B	ÖB	�'B	��B	�\B	�B	z�B	y�B	hsB	cTB	_;B	S�B	B�B	.B	bB��B�B�B��B��B��B��B��B��B��B�B�yB�ZB�BB�B��B��B��B��BɺBƨBĜBĜBÖB��B�}B�XB�FB�9B�'B�!B�B��B��B��B��B��B�hB�VB�+B�B�B�B�B�1B�7B�%B}�B~�B~�Bz�Bo�Bk�BgmBcTB^5B[#BZBYBXBW
BS�BN�BK�BG�BC�B@�B>wB:^B6FB2-B/B-B)�B&�B$�B#�B �B�B�B�B�B�B{BuBoB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B"�B"�B"�B"�B"�B#�B$�B$�B&�B(�B'�B&�B%�B%�B(�B.B1'B6FB;dB?}B@�BC�BH�BJ�BJ�BJ�BK�BL�BL�BN�BQ�BT�BVBXBZB\)B_;BcTBffBgmBgmBgmBhsBiyBk�Bo�Bp�Br�Br�Bt�Bu�Bv�Bx�Bx�B{�B}�B� B�B�%B�7B�PB�uB��B��B��B��B��B��B��B�B�LB�FB�LB�^B�wB��BÖBƨBɺB��B��B��B��B��B��B��B�B�B�5B�ZB�mB�sB�B�B��B��B��B��B��B	B	B	%B	+B	
=B	JB	VB	oB	�B	�B	�B	#�B	&�B	'�B	(�B	+B	.B	/B	2-B	33B	33B	49B	5?B	7LB	9XB	:^B	<jB	E�B	F�B	G�B	G�B	H�B	J�B	L�B	M�B	P�B	T�B	VB	XB	YB	YB	ZB	\)B	_;B	bNB	ffB	hsB	jB	k�B	k�B	n�B	n�B	n�B	p�B	p�B	r�B	u�B	|�B	~�B	�B	�B	�B	�%B	�7B	�=B	�=B	�=B	�=B	�DB	�=B	�1B	�1B	�+B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�9B	�FB	�LB	�XB	�dB	�dB	�dB	�jB	�qB	�wB	�wB	�}B	��B	��B	��B	ĜB	ŢB	ƨB	ƨB	ŢB	ŢB	ŢB	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�)B	�5B	�5B	�5B	�/B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�ZB	�`B	�fB	�mB	�sB	�sB	�yB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
+B
+B
	7B

=B
DB
DB
JB
VB
VB
VB
\B
VB
VB
VB
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
bB
hB
bB
hB
hB
oB
oB
oB
oB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
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
.B
.B
.B
.B
.B
/B
/B
0!B
0!B
0!B
1'B
33B
49B
49B
49B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
7LB
8RB
8RB
9XB
9XB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
>wB
?}B
@�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
D�B
E�B
E�B
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
J�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
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
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
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
T�B
T�B
T�B
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
YB
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
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
aHB
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
hsB
hsB
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
l�B
l�B
l�B
l�B
l�B
m�B
m�B
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
t�B
t�B
t�B
u�B
u�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�{B��B��B��B�B�.B�B"@B%�B$B$B%6B'eB&kB%gB%7B*yB6BN�B��B��B��B�aB��B�6B�ZB�nB�B��B��B�+B��B�OB��B��B��Bn�BahBc.BkeBz%B�=B�B��B�lB�]B��B��B�(B�1B�Bv�Bl�BZ9BDBB/�B�B*B�uB�AB�BB��B��B��B�cB��B�B�B��B�eB�2B�B�,B��B��B�hB�|B��B��B�AB{�BS�B9�B5B=yBJIB;�B#]B&B
�HB
�1B
�GB
�$B
��B
��B
�sB
�B
c�B
N�B
2�B
vB	�BB	�B	͌B	��B	��B	��B	��B	}IB	��B	j�B	e�B	dB	Y�B	J-B	:CB	�B	�B�gB�yB�5B��B�aB�%B��B��B��B��B�"B�yB�PB�1BּB��BͱB��B��B�wB�qB�TB�	B�rB�7B��B�lB�BB�vB�rB�qB�yB��B�oB��B��B�bB��B�SB�?B��B�kB��B��B��B�B�B�B��B�<Br�Bn�BlBj�BajB\�B[BY�BX�BZ�BX�BS"BO>BK\BG�BCBA�B>dB;5B6�B1}B0B.�B)dB)�B'}B"uB �B�BB�B�BiB�B1B]BKBvBB�B3B�B�B<BBjB#B�B}B�BB!�B!�B"�B#�B$fB#�B#�B$	B$�B%{B&�B'OB++B+~B*�B)HB(�B)�B,�B0�B3B9�B=�B@�BB�BF*BJ>BK+BK#BKLBL�BM�BN3BP:BS�BVVBW=BY B[�B]�B`9Bd|Bf�Bg�Bg�BhBi�BjCBl�Bp�BqBsBswBu�BvqBw?By@By�B|�B~�B�"B�qB�?B�lB��B�QB�<B�EB��B��B�4B�wB�3B�eB�uB�bB�-B�WB�-B��B�BǅBʇB�jB��B�QB�B��B�B�B֗B�GB�B��B�_B�B��B��B�B��B�B��B��B	�B	�B	�B	�B	�B	B	B		B	�B	�B	%B	$tB	'�B	)B	)�B	*�B	.B	/WB	2bB	3vB	3hB	4tB	6qB	8cB	:2B	;KB	<�B	FPB	G!B	G�B	HHB	I�B	KTB	M�B	O�B	RrB	UlB	V�B	X�B	Y�B	Y�B	[ B	]iB	`/B	b�B	gB	h�B	j�B	k�B	l�B	ofB	o B	o�B	qkB	q�B	sB	v B	}pB	�B	�B	��B	�jB	��B	�B	��B	��B	��B	��B	��B	�uB	��B	�B	�B	��B	��B	��B	��B	��B	�]B	��B	�UB	�HB	�0B	�B	�$B	�B	�MB	�B	�YB	�xB	��B	��B	�4B	��B	��B	�4B	��B	�LB	��B	��B	�"B	��B	�fB	��B	�$B	��B	��B	��B	�oB	��B	��B	�{B	��B	��B	��B	��B	��B	�KB	��B	��B	�4B	�'B	��B	��B	�7B	�8B	�B	�:B	��B	��B	��B	��B	��B	�*B	�B	�KB	�"B	�^B	�JB	ƙB	ȽB	˪B	�:B	ЁB	��B	ҌB	�:B	ҜB	�NB	ԁB	�MB	�KB	�KB	�vB	�FB	ըB	�`B	�kB	׍B	�rB	��B	޻B	��B	�_B	ݷB	��B	��B	��B	߻B	��B	��B	��B	�B	�B	��B	�B	�B	��B	��B	�B	�B	�[B	��B	��B	��B	�B	�B	�9B	�B	��B	�B	�#B	�HB	�sB	�_B	�3B	�B	�B	�#B	�%B	�AB	�cB	�\B	�<B	�YB	�bB	�sB	�cB
 aB
 TB
ZB
"B
iB
�B
vB
	�B

�B
�B
�B
�B
�B
�B
�B
-B
�B
B
2B
+B
�B

B
�B
$B
B
B
�B
�B

B
�B
B
 B
7B
B
B
;B
�B
%B
B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
B
#B
DB
HB
B
�B
3B
sB
B
+B
CB
fB
B
*B
B
 B
B
(B
B
B
B
B
�B
B
QB
*B
B
B
B
B
B
B
B
B
�B
�B
B
B
CB
OB
3B
cB
"B
 
B
 B
 B
 B
 B
 B
 .B
 9B
 TB
!UB
!(B
!B
!B
"B
""B
",B
"B
"B
":B
"hB
"zB
#vB
$dB
$-B
$UB
$FB
%3B
%YB
%TB
%�B
&�B
'UB
'MB
([B
(xB
(�B
(mB
(xB
)rB
)cB
)`B
)GB
*ZB
*SB
*�B
*eB
*�B
*�B
+�B
,�B
,kB
,�B
-�B
-rB
-�B
.gB
.]B
.\B
.hB
.tB
.�B
.mB
.�B
.�B
/�B
/�B
0�B
0�B
0�B
1�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
5�B
6 B
6�B
6�B
7GB
83B
9"B
9B
:+B
:]B
<B
;�B
;�B
;�B
;�B
<�B
=B
=DB
>6B
?<B
@VB
AMB
BB
A�B
BB
CcB
DB
D B
DYB
E`B
FFB
FHB
G2B
GB
HB
HB
HB
HB
H/B
H8B
I7B
IWB
KEB
LB
L&B
M!B
M!B
M#B
M1B
M:B
L%B
LB
L%B
MB
M-B
M]B
MGB
NAB
N6B
NsB
N5B
N5B
N]B
O.B
O0B
NAB
N7B
N\B
NfB
N�B
N}B
N5B
NB
NB
NB
N(B
NB
N)B
NB
O;B
OQB
O;B
P>B
P5B
P$B
P5B
P5B
P6B
PMB
QwB
QcB
Q9B
Q:B
R6B
RAB
ReB
RBB
ROB
RoB
SnB
SVB
S�B
SoB
TfB
TgB
T~B
TYB
TZB
TYB
UwB
UjB
U`B
VNB
VNB
V[B
WQB
W\B
W^B
WsB
X?B
XeB
Y}B
Y�B
Z�B
Z~B
Z�B
Z�B
Z�B
[�B
[�B
\�B
\�B
]�B
]�B
]�B
^ B
^�B
^�B
_�B
_�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
b�B
cB
c�B
c�B
c�B
c�B
dB
c�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
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
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
i�B
i�B
j�B
j�B
j�B
j�B
k�B
k�B
k�B
k�B
lB
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
l�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
nB
nB
m�B
nB
n�B
n�B
oB
n�B
o�B
p B
pEB
pB
p&B
o�B
pB
p&B
qB
qB
qB
p�B
p�B
qB
q*B
rB
rKB
q�B
q�B
rB
rB
rB
rB
r B
sB
sB
s#B
s=B
s#B
sCB
tB
s�B
s�B
s�B
s�B
tB
s�B
s�B
t"B
tB
uB
uB
uB
uB
uB
uB
uB
uB
u"B
uLB
u<B
v/B
v$B
v111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��C<3�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<$B�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<2k<#�
<#�<% �<#�
<#�
<$4�<32:<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Sss<(r�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - (REPORTED_SURFACE_PRESSURE) for PADJ                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al,2007,JAOT & effects of pressure adjustments                                                                                                                                                            PADJ REPORTED_SURFACE_PRESSURE =0.55 dbar                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            CTM alpha = 0.141 & tau = 6.68 s with error equal to the correction                                                                                                                                                                                             Pressures adjusted using reported surface pressure when warranted.                                                                                                                                                                                              none                                                                                                                                                                                                                                                            Salinity corrected for pressure sensor calibration drift and cell thermal mass                                                                                                                                                                                  201603101135222016031011352220160310113522  AO  ARCAADJP                                                                    20140721233942    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721233942  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721233942  QCF$                G�O�G�O�G�O�0                                                                                                                                   G�O�G�O�G�O�                PM  ARSQPADJV1.1                                                                20160310113522  QC  PRES            @�33D��3G�O�                PM  ARSQCTM V1.1                                                                20160310113522  QC  PSAL            @�33D��3G�O�                PM  ARSQOWGUV1.0WOD + Argo                                                      20170523133346  IP                  G�O�G�O�G�O�                