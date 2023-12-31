CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-23T09:15:46Z AOML 3.0 creation; 2016-05-31T19:14:42Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150323091546  20160531121442  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               kA   AO  4051_7090_107                   2C  D   APEX                            5368                            041511                          846 @�C��@1   @�C���`@3�bM���dkdZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    kA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyY�D���D�P D��fD���D�	�D�<�D��3D�� D� D�I�D���Dǹ�D�fD�VfD�vfD�ɚD��3D�<�D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��R@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B=qB=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸B���B�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��DtmqDyMqD��D�I�D��RD�ֹD��D�6�D��D���D�	�D�C�D���Dǳ�D� RD�PRD�pRD�ÆD��D�6�D�m1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ĜAŃA��A��HAĬAė�Ać+A�z�A�p�A�n�A�hsA�bNA�^5A�XA�XA�VA�S�A�-A��TA�A�  A��A��A��A���A���A��
A�AÕ�A�C�A�A��`A��A��9A�&�A�v�A��uA�A�  A���A�  A��mA��wA��+A�n�A�"�A���A�Q�A��yA�A�9XA�~�A�l�A�A��jA��-A�S�A�$�A��jA�{A���A�A���A��TA�A�A��FA�ZA��A�/A���A�n�A�9XA�G�A���A�ȴA�1A��-A�+A�ȴA�9XA�C�A���A�n�A��DA�t�A�n�A�A��RA�G�A�l�A���A��A���A�I�A�x�A�n�A��A���A��uA��A�  A�A�K�A~$�A|�RAwoApĜAk\)Ah��AgC�Ab�RAaXA^$�A[p�AXȴAV��AU/AR��AQ`BAP��AP�AP(�AO�hAOVANZAM`BAL5?AK%AJM�AH�RAG��AF��AE�hAC��AC�AB~�ABAAdZAA33A?hsA=dZA;XA:�A8�yA8A6��A5VA3�
A0��A. �A-�A,v�A+t�A)��A(�\A'XA&�+A$�`A#�A"��A!7LA�A��AoAS�AffA�;A�A�mA-A�`A�AȴAA&�A��A�At�A?}A�A��A�!A+A�/A�#A?}A
ĜA
5?A	oAM�A��AdZA~�A �A��A9XA�/A��A�wA��A`BA �`A 9X@��@��+@��#@�?}@��@�(�@��y@�-@��@�z�@���@�;d@���@�7L@��H@�-@�h@�@�1'@�  @��@��@�@���@�x�@�?}@���@���@��@��@�\)@�-@㕁@��@�1'@ߥ�@߅@�+@�?}@۝�@��@��@ڧ�@�M�@ؼj@��m@�|�@�V@�hs@�ƨ@�{@ѩ�@Л�@�j@��@�o@��@��@�1'@���@�?}@�9X@ǥ�@ǝ�@�E�@���@�9X@��;@Ý�@�l�@�t�@�dZ@�
=@�~�@���@�@���@��h@�1'@�@�^5@�~�@���@�@��;@��P@�S�@�33@�C�@�33@�;d@�dZ@�"�@��@���@���@���@���@���@�=q@��-@�`B@�G�@�hs@�G�@�7L@�/@���@���@��m@�ƨ@�"�@�ff@�M�@��@���@�~�@��+@��@�&�@��
@�ff@�~�@���@�dZ@�K�@�+@���@�&�@���@��@�z�@�j@��@��@�z�@�r�@��F@���@��F@�
=@���@�n�@���@��7@�O�@��`@��j@�r�@��@���@��y@��+@�n�@�M�@�M�@�E�@�5?@��@��^@�O�@��D@�|�@���@�~�@���@��7@�?}@�G�@�`B@�p�@�p�@�`B@�&�@��/@�j@�Q�@�I�@��;@�ȴ@���@��!@��@���@�{@�&�@��u@�j@�Z@��D@��@��@��u@�  @�ƨ@��@��@��@��D@�I�@��@���@�dZ@�\)@�;d@��y@���@�ff@���@�G�@��@�Ĝ@��9@���@�Z@��;@���@�\)@��@��@���@��!@���@��\@�v�@�ff@�5?@�$�@�$�@��@��@��T@���@��-@�`B@���@�j@� �@��@��
@��P@�33@��y@�v�@�-@�{@��^@���@���@��@�  @��;@���@�ƨ@��w@��F@���@�|�@�+@��@��!@�n�@�E�@�J@���@�@��7@��@��/@���@���@��D@�r�@�A�@��@���@��@���@���@�K�@���@�ȴ@��+@�v�@��T@��@�$�@y��@kS�@`�@XĜ@R^5@I��@B�@;"�@4Z@.@'�@!x�@5?@J@�@9X@Ĝ@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�ĜAŃA��A��HAĬAė�Ać+A�z�A�p�A�n�A�hsA�bNA�^5A�XA�XA�VA�S�A�-A��TA�A�  A��A��A��A���A���A��
A�AÕ�A�C�A�A��`A��A��9A�&�A�v�A��uA�A�  A���A�  A��mA��wA��+A�n�A�"�A���A�Q�A��yA�A�9XA�~�A�l�A�A��jA��-A�S�A�$�A��jA�{A���A�A���A��TA�A�A��FA�ZA��A�/A���A�n�A�9XA�G�A���A�ȴA�1A��-A�+A�ȴA�9XA�C�A���A�n�A��DA�t�A�n�A�A��RA�G�A�l�A���A��A���A�I�A�x�A�n�A��A���A��uA��A�  A�A�K�A~$�A|�RAwoApĜAk\)Ah��AgC�Ab�RAaXA^$�A[p�AXȴAV��AU/AR��AQ`BAP��AP�AP(�AO�hAOVANZAM`BAL5?AK%AJM�AH�RAG��AF��AE�hAC��AC�AB~�ABAAdZAA33A?hsA=dZA;XA:�A8�yA8A6��A5VA3�
A0��A. �A-�A,v�A+t�A)��A(�\A'XA&�+A$�`A#�A"��A!7LA�A��AoAS�AffA�;A�A�mA-A�`A�AȴAA&�A��A�At�A?}A�A��A�!A+A�/A�#A?}A
ĜA
5?A	oAM�A��AdZA~�A �A��A9XA�/A��A�wA��A`BA �`A 9X@��@��+@��#@�?}@��@�(�@��y@�-@��@�z�@���@�;d@���@�7L@��H@�-@�h@�@�1'@�  @��@��@�@���@�x�@�?}@���@���@��@��@�\)@�-@㕁@��@�1'@ߥ�@߅@�+@�?}@۝�@��@��@ڧ�@�M�@ؼj@��m@�|�@�V@�hs@�ƨ@�{@ѩ�@Л�@�j@��@�o@��@��@�1'@���@�?}@�9X@ǥ�@ǝ�@�E�@���@�9X@��;@Ý�@�l�@�t�@�dZ@�
=@�~�@���@�@���@��h@�1'@�@�^5@�~�@���@�@��;@��P@�S�@�33@�C�@�33@�;d@�dZ@�"�@��@���@���@���@���@���@�=q@��-@�`B@�G�@�hs@�G�@�7L@�/@���@���@��m@�ƨ@�"�@�ff@�M�@��@���@�~�@��+@��@�&�@��
@�ff@�~�@���@�dZ@�K�@�+@���@�&�@���@��@�z�@�j@��@��@�z�@�r�@��F@���@��F@�
=@���@�n�@���@��7@�O�@��`@��j@�r�@��@���@��y@��+@�n�@�M�@�M�@�E�@�5?@��@��^@�O�@��D@�|�@���@�~�@���@��7@�?}@�G�@�`B@�p�@�p�@�`B@�&�@��/@�j@�Q�@�I�@��;@�ȴ@���@��!@��@���@�{@�&�@��u@�j@�Z@��D@��@��@��u@�  @�ƨ@��@��@��@��D@�I�@��@���@�dZ@�\)@�;d@��y@���@�ff@���@�G�@��@�Ĝ@��9@���@�Z@��;@���@�\)@��@��@���@��!@���@��\@�v�@�ff@�5?@�$�@�$�@��@��@��T@���@��-@�`B@���@�j@� �@��@��
@��P@�33@��y@�v�@�-@�{@��^@���@���@��@�  @��;@���@�ƨ@��w@��F@���@�|�@�+@��@��!@�n�@�E�@�J@���@�@��7@��@��/@���@���@��D@�r�@�A�@��@���@��@���@���@�K�@���@�ȴ@��+@�v�@��T@��@�$�@y��@kS�@`�@XĜ@R^5@I��@B�@;"�@4Z@.@'�@!x�@5?@J@�@9X@Ĝ@$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  B\B9XBH�BbNBy�B��B�wB�mBB�B%�B&�B"�B+BD�B;dB8RB:^B=qB5?B-BPB�B�B��B��B��B�9B�?B��B�^B��B�;B�HB�/B�BB��B��B��B��B��BƨB�}B�-B��B�oB�Bt�Bs�Bs�BjBD�B=qBE�BL�B@�B.B#�B�B�BJBB�B�NB�#B��B��B�!B��B��B�oB|�BbNBYBXBQ�B:^B�B	7B
��B
�ZB
��B
��B
\)B
>wB
!�B
DB	��B	�
B	�-B	�uB	�B	x�B	cTB	ZB	Q�B	E�B	6FB	(�B	!�B	�B	\B	JB	DB		7B	+B	B	B	B	%B	B	B��B��B��B�B�B�sB�fB�ZB�fB�mB�yB�mB�HB�/B�B��B��B��BĜB�^B�9B�'B�B��B��B��B��B�bB�+Bz�Bs�Bk�BbNB]/B[#BXBVBT�B\)BaHBhsBl�BjBm�BjBiyBk�Bq�Bq�Bq�Bq�Bp�Bn�Bl�Bl�Bl�Bk�Bk�BjBk�Bl�Bm�Bm�Bm�Bm�Bo�Bq�Bo�Bn�Bm�Bm�Bl�BjBjBk�Bk�Bl�Bl�Bl�Bl�Bl�Bl�Bn�Bn�Bn�Bo�Bo�Bs�Bw�By�Bz�B|�B|�B{�B{�B{�B|�B}�B}�B}�B~�B�B�B�+B�1B�1B�B~�B~�B}�B}�B}�B� B�B�+B�+B�1B�7B�PB�\B�bB�bB�\B�bB�oB�oB��B��B��B��B��B��B��B��B�B�B�B�9B�FB�3B�?B�RB�^B�jB�qB�wB��BBƨBɺB��B��B��B��B��B��B�B�B��B��B�B�B�B�#B�BB�mB�B�B�B�B�B�B��B��B��B	  B	B	VB	\B	bB	bB	\B	bB	bB	bB	hB	{B	�B	�B	#�B	-B	/B	.B	-B	.B	1'B	5?B	8RB	;dB	;dB	;dB	;dB	:^B	<jB	@�B	A�B	B�B	E�B	G�B	K�B	N�B	T�B	W
B	YB	]/B	_;B	`BB	e`B	gmB	hsB	jB	jB	k�B	l�B	n�B	p�B	s�B	s�B	t�B	u�B	u�B	u�B	x�B	y�B	z�B	~�B	�B	�B	�%B	�7B	�DB	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�LB	�XB	�^B	�jB	�wB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�#B	�)B	�)B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�TB	�ZB	�fB	�fB	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
PB
�B
!�B
+B
33B
:^B
?}B
E�B
J�B
P�B
T�B
[#B
aHB
ffB
iyB
m�B
p�B
r�B
w�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�
B�B�B�B�B�B�	B�	B�B�B�B�B�B B B BkB9gBH�Bb]By�B��B��B�B2B�B%�B' B"�B+BD�B;xB8dB:oB=�B5TB-"BcB��B�B�
B��B�B�MB�SB��B�nB��B�MB�YB�BB�SB��B��B��B�B�
BƾB��B�@B��B��B�Bt�Bs�Bs�Bj�BD�B=�BE�BL�B@�B.&B#�B�B�B[B,B�B�\B�7B��B��B�6B��B��B��B} Bb_BY+BX!BQ�B:pB�B	LB
��B
�nB
��B
��B
\@B
>�B
!�B
`B	�	B	�(B	�KB	��B	�8B	x�B	cwB	Z?B	RB	E�B	6gB	)B	!�B	�B	�B	mB	hB		]B	OB	DB	AB	AB	JB	>B	,B�B�B��B��B�B�B�B�B�B�B�B�B�qB�VB�>B�%B�	B��B��B��B�bB�PB�9B�B��B��B��B��B�YB{Bs�Bk�BbzB]\B[SBX>BV3BU*B\WBayBh�Bl�Bj�Bm�Bj�Bi�Bk�Bq�Bq�Bq�Bq�Bp�Bn�Bl�Bl�Bl�Bk�Bk�Bj�Bk�Bl�Bm�Bm�Bm�Bm�Bo�Bq�Bo�Bn�Bm�Bm�Bl�Bj�Bj�Bk�Bk�Bl�Bl�Bl�Bl�Bl�Bl�Bn�Bn�Bn�Bo�Bo�Bs�Bw�BzB{B}B}B|B|B|B}B~!B~B~ B)B�3B�GB�YB�_B�_B�8B*B(B~"B~B~ B�.B�JB�YB�VB�\B�cB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�2B�?B�BB�dB�qB�\B�lB�}B��B��B��B��B��B¹B��B��B��B�B�B��B�B�B�BB�9B�'B�)B�-B�:B�AB�JB�lB�B�B�B��B��B��B��B��B��B�B	 &B	9B	}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	-5B	/@B	.9B	-5B	.:B	1LB	5aB	8xB	;�B	;�B	;�B	;�B	:�B	<�B	@�B	A�B	B�B	E�B	G�B	K�B	N�B	U!B	W.B	Y=B	]RB	_^B	`gB	e�B	g�B	h�B	j�B	j�B	k�B	l�B	n�B	p�B	s�B	s�B	t�B	u�B	u�B	u�B	x�B	y�B	{B	B	�7B	�CB	�FB	�XB	�hB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�2B	�JB	�nB	�yB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	� B	�B	�#B	�5B	�BB	�HB	�IB	�NB	�XB	�[B	�dB	�bB	�iB	�oB	�tB	�rB	�zB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B
 #B
 B
 B
'B
&B
&B
%B
'B
&B
&B
%B
-B
,B
3B
2B
2B
=B
oB
�B
!�B
+"B
3SB
:|B
?�B
E�B
J�B
Q B
UB
[AB
acB
f�B
i�B
m�B
p�B
r�B
w�B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214422016053112144220160531121442  AO  ARCAADJP                                                                    20150323091546    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150323091546  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150323091546  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121442  IP                  G�O�G�O�G�O�                