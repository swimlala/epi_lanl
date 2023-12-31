CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-03T02:15:23Z AOML 3.0 creation; 2016-05-31T19:14:43Z UW 3.1 conversion     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150603021523  20160531121443  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               rA   AO  4051_7090_114                   2C  D   APEX                            5368                            041511                          846 @�Ur��1   @�Us�?�@4�\(��dDr� Ĝ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    rA   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffBffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�fD�VfD���D���D��D�I�D���D��fD�3D�@ D�|�D�� D�fD�Y�Dړ3D��fD�3D�33D�p D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @z=p@��@��A��A<��A\��A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�G�B��B=qB=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)��C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEs�DE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR��DSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Das�Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt�Dy��D� RD�PRD���D���D��D�C�D���D��RD�D�9�D�v�D���D� RD�S�DڍD��RD��D�-D�i�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��HA���A���A�ƨAѺ^AѬAї�A��AХ�AБhA�ZA�(�A�
=A��yA��;A���A��#A�oA�z�A���A���A͇+A��A�(�A���Aʇ+A�x�A��A� �A�bNAőhA�5?A�1'A���Aĝ�A��mA�\)A�&�A��A���A®A�z�A�7LA��A� �A�  A��!A�"�A�ƨA�-A��A��A��wA�ZA�1'A��mA��FA���A��uA�O�A���A�p�A� �A���A�ffA���A�bNA�G�A�oA�p�A�^5A�1'A���A���A�VA�t�A���A��FA�VA�C�A�hsA��A��7A��FA�`BA�E�A���A� �A�\)A��A���A�9XA��A�XA�O�A���A��A��A�VA��A���A��RA��;A�9XA�S�A�VA���A�jA�
=A���A�ffA�p�A�ƨA���A�Q�A��A��RA�C�A�&�A�~�A��uA�bNA�1A�v�A�=qA���A�%A�1A��A���A�/A��-A�ffA�r�A��^A�C�A�E�A�A�A~�9Az�uAw33Av9XAsO�Ap�jAn�yAl1Aj$�Ag�Ac"�A`��A^��A]�hA[p�A["�AY��AT��ASp�AR��AQƨAO�TAL��AJ�/AI�PAGAEhsAC33AAp�A@�!A?��A>r�A=��A<�A:�!A6�A4��A2�uA0�A/K�A.z�A-�A+�A)�
A)hsA(�A'��A&��A&{A$�9A#�
A"�A!��A!
=A �9Al�A��A�AZA;dAȴAv�AM�A1'A1AO�A�A�TAdZA�A��A�A^5A5?A��AA�A��A��AdZA�A	�mA	+A	33A	?}A	G�A	�A	oA�RAM�A��A�DA�AƨAQ�AdZA��A��A�;AjA ��@��@��-@��@�K�@�ff@��h@�j@�o@�Z@�
=@�p�@��H@���@���@�ȴ@�P@�hs@��/@���@�n�@�/@۾w@��@ָR@�J@�@ԃ@�\)@�ȴ@�~�@���@�hs@���@�1'@·+@�7L@���@̃@�I�@˝�@��y@ɑh@�(�@�\)@��H@ƸR@�n�@�`B@þw@�v�@�J@���@��7@��@��;@�"�@�n�@�J@��T@���@��@�O�@���@�Ĝ@�I�@��;@�;d@�~�@�ff@��@��@�%@���@�r�@� �@��m@���@��F@�l�@�"�@��@���@�v�@�E�@��@��@�X@���@�bN@��P@�+@���@�~�@�J@���@��7@�?}@��@��/@���@� �@��m@���@�33@�o@���@��@���@��R@��+@�^5@�E�@��@���@�?}@���@���@��9@��9@��@��u@�r�@�I�@���@�+@��@���@�ff@�5?@��T@�hs@��@��@��u@�r�@�9X@��@���@�K�@�
=@�ȴ@�E�@�5?@�-@�J@��@�@��-@��h@��7@��@��@� �@��m@���@���@��@��@�dZ@�"�@��@���@�n�@�=q@��@�@�X@��@�Ĝ@�bN@���@���@�\)@�
=@���@�$�@���@���@���@��@�`B@��@��`@�Ĝ@��@�1'@���@���@��;@��@���@�  @�1@�  @���@��P@�|�@��@��!@��\@�V@��@��@�@��@��`@���@��@�j@�9X@���@��@���@��@�l�@�;d@��@��@��R@�~�@��^@��@���@�I�@�1@���@�C�@�
=@�ȴ@��+@�=q@�J@��^@���@�x�@�7L@��/@�r�@�1'@�ƨ@�\)@��y@���@��+@��@��^@��-@���@���@��@�hs@��P@�\)@�X@s��@dI�@Z^5@Qx�@KS�@D��@@�@;ƨ@4�@-��@&{@!��@�h@�7@�@��@?}@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��HA���A���A�ƨAѺ^AѬAї�A��AХ�AБhA�ZA�(�A�
=A��yA��;A���A��#A�oA�z�A���A���A͇+A��A�(�A���Aʇ+A�x�A��A� �A�bNAőhA�5?A�1'A���Aĝ�A��mA�\)A�&�A��A���A®A�z�A�7LA��A� �A�  A��!A�"�A�ƨA�-A��A��A��wA�ZA�1'A��mA��FA���A��uA�O�A���A�p�A� �A���A�ffA���A�bNA�G�A�oA�p�A�^5A�1'A���A���A�VA�t�A���A��FA�VA�C�A�hsA��A��7A��FA�`BA�E�A���A� �A�\)A��A���A�9XA��A�XA�O�A���A��A��A�VA��A���A��RA��;A�9XA�S�A�VA���A�jA�
=A���A�ffA�p�A�ƨA���A�Q�A��A��RA�C�A�&�A�~�A��uA�bNA�1A�v�A�=qA���A�%A�1A��A���A�/A��-A�ffA�r�A��^A�C�A�E�A�A�A~�9Az�uAw33Av9XAsO�Ap�jAn�yAl1Aj$�Ag�Ac"�A`��A^��A]�hA[p�A["�AY��AT��ASp�AR��AQƨAO�TAL��AJ�/AI�PAGAEhsAC33AAp�A@�!A?��A>r�A=��A<�A:�!A6�A4��A2�uA0�A/K�A.z�A-�A+�A)�
A)hsA(�A'��A&��A&{A$�9A#�
A"�A!��A!
=A �9Al�A��A�AZA;dAȴAv�AM�A1'A1AO�A�A�TAdZA�A��A�A^5A5?A��AA�A��A��AdZA�A	�mA	+A	33A	?}A	G�A	�A	oA�RAM�A��A�DA�AƨAQ�AdZA��A��A�;AjA ��@��@��-@��@�K�@�ff@��h@�j@�o@�Z@�
=@�p�@��H@���@���@�ȴ@�P@�hs@��/@���@�n�@�/@۾w@��@ָR@�J@�@ԃ@�\)@�ȴ@�~�@���@�hs@���@�1'@·+@�7L@���@̃@�I�@˝�@��y@ɑh@�(�@�\)@��H@ƸR@�n�@�`B@þw@�v�@�J@���@��7@��@��;@�"�@�n�@�J@��T@���@��@�O�@���@�Ĝ@�I�@��;@�;d@�~�@�ff@��@��@�%@���@�r�@� �@��m@���@��F@�l�@�"�@��@���@�v�@�E�@��@��@�X@���@�bN@��P@�+@���@�~�@�J@���@��7@�?}@��@��/@���@� �@��m@���@�33@�o@���@��@���@��R@��+@�^5@�E�@��@���@�?}@���@���@��9@��9@��@��u@�r�@�I�@���@�+@��@���@�ff@�5?@��T@�hs@��@��@��u@�r�@�9X@��@���@�K�@�
=@�ȴ@�E�@�5?@�-@�J@��@�@��-@��h@��7@��@��@� �@��m@���@���@��@��@�dZ@�"�@��@���@�n�@�=q@��@�@�X@��@�Ĝ@�bN@���@���@�\)@�
=@���@�$�@���@���@���@��@�`B@��@��`@�Ĝ@��@�1'@���@���@��;@��@���@�  @�1@�  @���@��P@�|�@��@��!@��\@�V@��@��@�@��@��`@���@��@�j@�9X@���@��@���@��@�l�@�;d@��@��@��R@�~�@��^@��@���@�I�@�1@���@�C�@�
=@�ȴ@��+@�=q@�J@��^@���@�x�@�7L@��/@�r�@�1'@�ƨ@�\)@��y@���@��+@��@��^@��-@���@���@��@�hs@��P@�\)@�X@s��@dI�@Z^5@Qx�@KS�@D��@@�@;ƨ@4�@-��@&{@!��@�h@�7@�@��@?}@
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
J�B
J�B
J�B
J�B
J�B
J�B
I�B
K�B
T�B
aHB
}�B
��B
��B
��B
��B
�B
�FB
�TBF�B��B�HB�B{B�B6FB^5B\)BZBYB]/BhsBjBl�Bs�B�hB�qBÖB�B��BoB�B"�B%�B,B0!BA�BL�BW
BcTBs�B�%B�=B�DB�\B��B��B��B�B�!B�B�3B�?B�LB�'B�B��B��B��B��B��B��B��B��B�uB�VB�+B� B~�Bw�Bk�BiyBdZB`BBgmBiyBhsBhsBjB`BBS�BW
BG�B-B�B	7B��BB%BDB{B�BbB	7B  B�B�B�wB�}B�RB�-B�'B��B�B|�Br�B`BBG�B!�B�B�fB�BB�XB��B�bB�=B~�Bm�BXB8RB{B1B
��B
�`B
��B
ǮB
�qB
��B
��B
u�B
T�B
;dB
.B
�B
+B	��B	�fB	��B	�FB	��B	��B	�bB	�1B	{�B	v�B	m�B	T�B	N�B	J�B	D�B	9XB	,B	 �B	�B	PB	B��B�B�B�mB�HB�/B�
B��BÖB�}B�dB�RB�FB�9B�!B�B��B��B��B��B��B��B��B�hB�DB�+B�B�B{�Bw�Bt�Bq�Bo�Bn�Bm�Bl�Bl�Bk�Bl�Br�Bs�Bq�Bp�Bp�Bo�B�%B�{B�bB�PB�=B�+B�7B�bB��B��B��B�!B�dB�}BŢB��B��B��B��B�
B�;B�B�
B�)B�BB�fB�ZB�/B�B�B��B��B��B��B��BǮBÖB�}B�jB�?B�-B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�3B�9B�FB�XB�dB�jB�jB�qB�qB�qB�}BŢB��B��B��B��B��B�B�/B�NB�sB�B�B�B�B��B��B��B��B��B	B	B		7B	\B	bB	oB	�B	�B	�B	�B	 �B	"�B	%�B	'�B	+B	,B	-B	/B	0!B	5?B	8RB	;dB	@�B	A�B	B�B	C�B	E�B	F�B	G�B	I�B	J�B	K�B	L�B	O�B	P�B	R�B	T�B	T�B	VB	VB	W
B	W
B	YB	YB	ZB	ZB	\)B	_;B	aHB	bNB	cTB	cTB	cTB	cTB	dZB	e`B	gmB	l�B	m�B	n�B	o�B	o�B	q�B	s�B	u�B	w�B	w�B	x�B	y�B	z�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�7B	�1B	�7B	�PB	�VB	�\B	�\B	�bB	�\B	�bB	�oB	�oB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�LB	�XB	�^B	�jB	�jB	�qB	��B	B	ÖB	ǮB	��B	��B	��B	�B	�B	�B	�#B	�#B	�5B	�;B	�;B	�BB	�HB	�NB	�`B	�fB	�fB	�fB	�mB	�mB	�yB	�yB	�yB	�sB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
B
DB
oB
�B
 �B
)�B
2-B
9XB
@�B
C�B
G�B
N�B
S�B
[#B
bNB
e`B
jB
n�B
r�B
v�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
J�B
J�B
J�B
J�B
J�B
J�B
I�B
K�B
UB
a\B
~
B
��B
��B
��B
�	B
�(B
�YB
�eBF�B��B�WB�B�B�B6WB^HB\;BZ)BY)B]=Bh�Bj�Bl�Bs�B�wB��BèB�B��B�B�B"�B%�B,B02BA�BL�BWBckBs�B�;B�PB�ZB�rB��B��B��B�/B�8B�)B�IB�VB�dB�=B�"B�B�B�B�B��B��B��B��B��B�mB�BB�BBw�Bk�Bi�BdpB`WBg�Bi�Bh�Bh�Bj�B`XBTBW"BG�B-#B�B	IB�BB9BVB�B�ByB	JB B�B�B��B��B�hB�<B�;B��B� B}Br�B`SBG�B!�B��B�wB�UB�iB��B�tB�RBBm�BX&B8hB�BDB
�B
�sB
�B
��B
��B
�B
��B
u�B
UB
;~B
./B
�B
HB	��B	�B	�B	�aB	��B	��B	��B	�PB	|	B	v�B	m�B	U!B	N�B	J�B	D�B	9yB	,+B	 �B	�B	uB	AB�B��B�B�B�oB�UB�3B��BüB��B��B�zB�nB�bB�KB�:B�(B�B�B��B��B��B��B��B�oB�YB�EB�:B|Bw�Bt�Bq�Bo�Bn�Bm�Bl�Bl�Bk�Bl�Br�Bs�Bq�Bp�Bp�Bo�B�RB��B��B�}B�jB�YB�cB��B��B��B�'B�IB��B��B��B��B��B��B��B�1B�dB�.B�3B�SB�jB�B�B�XB�@B�,B�B�B�B��B��B��B��B��B��B�lB�XB�CB�6B�)B�'B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B�JB�^B�dB�rB��B��B��B��B��B��B��B��B��B��B��B�B�	B�B�:B�YB�xB�B�B��B��B��B��B��B�B�B�B	CB	CB		`B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	&B	(B	+)B	,.B	-3B	/AB	0IB	5gB	8vB	;�B	@�B	A�B	B�B	C�B	E�B	F�B	G�B	I�B	J�B	K�B	L�B	PB	Q
B	SB	U!B	U"B	V'B	V*B	W/B	W/B	Y<B	Y;B	ZAB	ZCB	\OB	__B	alB	bqB	cwB	cwB	cxB	cyB	d~B	e�B	g�B	l�B	m�B	n�B	o�B	o�B	q�B	s�B	u�B	w�B	w�B	x�B	y�B	{B	}B	~B	�#B	�+B	�5B	�8B	�6B	�<B	�AB	�GB	�OB	�UB	�ZB	�RB	�YB	�tB	�vB	�~B	�B	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	� B	�%B	�(B	�6B	�HB	�_B	�lB	�zB	�B	��B	��B	��B	��B	°B	öB	��B	��B	�B	�B	�$B	�1B	�>B	�CB	�CB	�YB	�[B	�_B	�bB	�gB	�pB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B
$B
+B
2B
2B
0B
1B
1B
8B
8B
?B
CB
CB
KB
>B
eB
�B
�B
 �B
*B
2JB
9vB
@�B
C�B
G�B
N�B
TB
[@B
blB
e}B
j�B
n�B
r�B
v�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214432016053112144320160531121443  AO  ARCAADJP                                                                    20150603021523    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150603021523  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150603021523  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121443  IP                  G�O�G�O�G�O�                