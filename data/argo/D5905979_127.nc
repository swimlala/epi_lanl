CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-06-19T17:09:24Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �h   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �x   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �|   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200619170924  20220204114423  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��ܨ3�k1   @���5�<@7)x����b��\)1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @���@�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B ffBffBffB  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D���D�_\D���D���D�'\D�Z�D��=D��3D��D�[3D���D�ɚD�'�D�T�Dژ�D���D�(�D�\�D�\D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��A��A<��A[\)A|��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�G�B��B��B=qB=qB'=qB/=qB7=qB?=qBG=qBO=qBW=qB_=qBg=qBo=qBw=qB=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BÞ�BǞ�B˞�BϞ�BӞ�Bמ�B۞�Bߞ�B㞸B瞸B랸BB�k�B���B���B���C�\C�\C�\C�\C	�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C�\C!�\C#�\C%�\C'�\C)�\C+�\C-�\C/�\C1�\C3�\C5�\C7�\C9�\C;�\C=�\C?�\CA�\CC�\CE�\CG�\CI�\CK�\CM�\CO�\CQ�\CS�\CU�\CW�\CY�\C[�\C]�\C_�\Ca�\Cc�\Ce�\Cg�\Ci�\Ck�\Cm�\Co�\Cq�\Cs�\Cu�\Cw�\Cy�\C{�\C}�\C�\C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D s�D ��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D	s�D	��D
s�D
��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��Ds�D��D s�D ��D!s�D!��D"s�D"��D#s�D#��D$s�D$��D%s�D%��D&s�D&��D's�D'��D(s�D(��D)s�D)��D*s�D*��D+s�D+��D,s�D,��D-s�D-��D.s�D.��D/s�D/��D0s�D0��D1s�D1��D2s�D2��D3s�D3��D4s�D4��D5s�D5��D6s�D6��D7s�D7��D8s�D8��D9s�D9��D:s�D:��D;s�D;��D<s�D<��D=s�D=��D>s�D>��D?s�D?��D@s�D@��DAs�DA��DBs�DB��DCs�DC��DDs�DD��DEmqDE��DFs�DF��DGs�DG��DHs�DH��DIs�DI��DJs�DJ��DKs�DK��DLs�DL��DMs�DM��DNs�DN��DOs�DO��DPs�DP��DQs�DQ��DRs�DR�qDSs�DS��DTs�DT��DUs�DU��DVs�DV��DWs�DW��DXs�DX��DYs�DY��DZs�DZ��D[s�D[��D\s�D\��D]s�D]��D^s�D^��D_s�D_��D`s�D`��Daz=Da��Dbs�Db��Dcs�Dc��Dds�Dd��Des�De��Dfs�Df��Dgs�Dg��Dhs�Dh��Dis�Di��Djs�Dj��Dks�Dk��Dls�Dl��Dms�Dm��Dns�Dn��Dos�Do��Dps�Dp��Dqs�Dq��Drs�Dr��Dss�Ds��Dts�Dt��Dy��D��D�YHD���D�ǯD�!HD�T�D��)D��D��D�UD���D�ÆD�!�D�N�Dڒ�D���D�"�D�V�D�HD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AuA�A£�A®A§�A©�A�A���A���A���A���A¸RA¶FA¾wA¾wA¾wA¾wA¼jA���A���A¼jA�A�ĜA¾wA¾wA�A�ƨA�ȴA���A��
A���A��#A��/A��/A��/A��A���A���A���A���A�ȴA�ȴA�ƨA���A���A�ƨA���A���A�ĜA�ƨA�ƨA�ƨA���A¼jA�A�9XA�hsA���A��FA�bNA�VA��A�v�A�M�A���A�Q�A��wA���A�9XA�VA�%A��\A���A��PA��A�33A��yA�`BA�1A�x�A��A���A��A��mA�(�A��7A��mA�O�A��jA���A���A�r�A���A��`A��A���A��A���A�ƨA�7LA�`BA�+A���A�~�A��A��`A��A��`A��\A�;dA��A��jA�I�A�x�A���A�G�A���A��;A�+A���A�`BA}�wAx��Aw�PAt��AqO�ApbNAn�HAk�AiAf�Af=qAe
=Acp�Aa��A]�A[�AZ1'AYXAX�AW�FAW�AVM�ATȴAR�AP(�AM�;AK�wAK�AI�TAHZAGoAE|�AC�;ACC�AA��A@jA>~�A<��A;A:��A9S�A7��A7p�A6�9A4�9A2��A1p�A0ȴA/��A.��A.(�A-oA,  A*��A)dZA(��A(~�A'�A%��A$��A$�A#�A"�DA!��A!t�A!�^A!��A#�A"�DA!hsA bNA�HA�TAjA�`A�hAz�A1A��A1AbA1A��A��AJA1At�Ap�A33Al�A��A  A�9Ap�A�A�
A^5A1'A�^A
�jA
Q�A�PA��A?}A"�A�yA�mA	VA��A$�A��@��/@���@���@�`B@��-@���@�Z@���@�A�@�5?@�bN@��u@���@��@�9@@���@��@�l�@�n�@�z�@�Ĝ@��@�5?@�  @�n�@ޟ�@��T@ܼj@�;d@ٙ�@�Ĝ@��T@�V@��#@�dZ@�v�@���@�C�@ԋD@���@�Z@�z�@ϕ�@�\)@�~�@�@��
@��@��@�I�@��;@�S�@��h@���@��@�5?@��j@��
@�;d@�7L@���@���@��@�K�@�o@��+@��@�hs@��@�(�@�j@���@�bN@��P@���@��@�dZ@�S�@���@�5?@�^5@���@�M�@��@��7@�%@���@��@�I�@�;d@�v�@�ff@�E�@��^@��7@�?}@��u@��9@��@�1'@�\)@�"�@�
=@���@���@�b@�@�V@���@���@�^5@��@���@��j@�7L@�/@��u@��+@�~�@���@��7@�p�@�O�@���@�Ĝ@�(�@��w@�o@��R@�ȴ@�"�@�"�@���@��@�/@��@��/@�Ĝ@��u@�9X@���@���@��
@���@�C�@�K�@�K�@�;d@��y@���@���@��@�{@��@���@��T@���@���@���@��7@��@���@��^@��7@�?}@��D@� �@��m@��
@��w@��!@��h@�O�@�%@���@��D@�z�@�r�@�r�@�9X@�1@��
@��F@���@���@��@�l�@�C�@�33@��@��y@���@��+@�^5@�$�@�@��@���@��@�O�@�7L@���@��@�r�@�I�@��m@���@��
@�ƨ@�dZ@��w@���@�o@�@�o@��y@���@��+@�ff@�$�@��#@���@��h@�x�@�G�@���@���@��@��j@��9@�I�@��w@���@���@�\)@�"�@���@���@��\@�~�@�^5@�E�@�$�@��#@��h@�V@�Ĝ@��D@�j@�bN@���@�ƨ@���@��P@�K�@��@��$@}��@t��@m��@ep�@\~(@V��@P��@HA�@A#�@;��@6)�@/S@*�@#Mj@W�@��@_@V@N<@	;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AuA�A£�A®A§�A©�A�A���A���A���A���A¸RA¶FA¾wA¾wA¾wA¾wA¼jA���A���A¼jA�A�ĜA¾wA¾wA�A�ƨA�ȴA���A��
A���A��#A��/A��/A��/A��A���A���A���A���A�ȴA�ȴA�ƨA���A���A�ƨA���A���A�ĜA�ƨA�ƨA�ƨA���A¼jA�A�9XA�hsA���A��FA�bNA�VA��A�v�A�M�A���A�Q�A��wA���A�9XA�VA�%A��\A���A��PA��A�33A��yA�`BA�1A�x�A��A���A��A��mA�(�A��7A��mA�O�A��jA���A���A�r�A���A��`A��A���A��A���A�ƨA�7LA�`BA�+A���A�~�A��A��`A��A��`A��\A�;dA��A��jA�I�A�x�A���A�G�A���A��;A�+A���A�`BA}�wAx��Aw�PAt��AqO�ApbNAn�HAk�AiAf�Af=qAe
=Acp�Aa��A]�A[�AZ1'AYXAX�AW�FAW�AVM�ATȴAR�AP(�AM�;AK�wAK�AI�TAHZAGoAE|�AC�;ACC�AA��A@jA>~�A<��A;A:��A9S�A7��A7p�A6�9A4�9A2��A1p�A0ȴA/��A.��A.(�A-oA,  A*��A)dZA(��A(~�A'�A%��A$��A$�A#�A"�DA!��A!t�A!�^A!��A#�A"�DA!hsA bNA�HA�TAjA�`A�hAz�A1A��A1AbA1A��A��AJA1At�Ap�A33Al�A��A  A�9Ap�A�A�
A^5A1'A�^A
�jA
Q�A�PA��A?}A"�A�yA�mA	VA��A$�A��@��/@���@���@�`B@��-@���@�Z@���@�A�@�5?@�bN@��u@���@��@�9@@���@��@�l�@�n�@�z�@�Ĝ@��@�5?@�  @�n�@ޟ�@��T@ܼj@�;d@ٙ�@�Ĝ@��T@�V@��#@�dZ@�v�@���@�C�@ԋD@���@�Z@�z�@ϕ�@�\)@�~�@�@��
@��@��@�I�@��;@�S�@��h@���@��@�5?@��j@��
@�;d@�7L@���@���@��@�K�@�o@��+@��@�hs@��@�(�@�j@���@�bN@��P@���@��@�dZ@�S�@���@�5?@�^5@���@�M�@��@��7@�%@���@��@�I�@�;d@�v�@�ff@�E�@��^@��7@�?}@��u@��9@��@�1'@�\)@�"�@�
=@���@���@�b@�@�V@���@���@�^5@��@���@��j@�7L@�/@��u@��+@�~�@���@��7@�p�@�O�@���@�Ĝ@�(�@��w@�o@��R@�ȴ@�"�@�"�@���@��@�/@��@��/@�Ĝ@��u@�9X@���@���@��
@���@�C�@�K�@�K�@�;d@��y@���@���@��@�{@��@���@��T@���@���@���@��7@��@���@��^@��7@�?}@��D@� �@��m@��
@��w@��!@��h@�O�@�%@���@��D@�z�@�r�@�r�@�9X@�1@��
@��F@���@���@��@�l�@�C�@�33@��@��y@���@��+@�^5@�$�@�@��@���@��@�O�@�7L@���@��@�r�@�I�@��m@���@��
@�ƨ@�dZ@��w@���@�o@�@�o@��y@���@��+@�ff@�$�@��#@���@��h@�x�@�G�@���@���@��@��j@��9@�I�@��w@���@���@�\)@�"�@���@���@��\@�~�@�^5@�E�@�$�@��#@��h@�V@�Ĝ@��D@�j@�bN@���@�ƨ@���@��P@�K�G�O�@��$@}��@t��@m��@ep�@\~(@V��@P��@HA�@A#�@;��@6)�@/S@*�@#Mj@W�@��@_@V@N<@	;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
ɺB
ɺB
ɺB
ɺB
ɺB
ɺB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ǮB
ǮB
ǮB
ɺB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
ȴB
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�#B
�B
�#B
�#B
�#B
�B
�B
�#B
�#B
�#B
�BB
�BB49BO�Br�B�jB�B�B�
B��B��B�`B��BBPB%B+BoB+B6FB/B-B+B+B(�B"�B�B�B�B�B�B �B!�B&�B,B,B(�B%�B#�B�B�BhBJBB��B�B�NB�5B�B��B��BȴB�9B{�Bv�Bu�Bp�BjB_;BW
B;dBB
�^B
��B
cTB
2-B
�B	�B	�B	�
B	�FB	�B	��B	�bB	�B	p�B	jB	dZB	ZB	N�B	;dB	-B	"�B	�B	�B	�B	{B	PB	1B��B�B�TB�#B�#B�B��B��B��BƨBÖB�XB�!B��B��B��B�uB�PB�PB�7B�%B�+B�B� B}�B|�Bz�Bx�Bv�Bq�Bq�Bl�Bk�BhsBgmBcTBbNBaHB`BBaHB_;B^5Be`Bn�B�B�VB�7B�B|�Bv�Bo�Be`Bw�Bs�Bs�Bz�B�B�bB�%B�1B��B��B�B��B�hB�bB��B�oB�+B}�Bv�B�DB��B�B�?B��B�DB�1B��B�RB��B��B��BŢB�'B�+Bu�By�B`BBW
B]/B`BBdZBq�B�B�=B�bB�uB�DB�PB�+Bx�Br�Bk�Be`BaHBaHBbNBbNBjBiyBffB`BB_;BW
BYB]/B]/B^5BaHBq�B{�B{�Bv�Bx�B~�B�7B�Bz�Bw�B�B�=B�1B�=B�+B�B|�B{�B~�B~�B� B�1B�7B�=B�=B�JB�DB�DB�\B�{B��B��B��B��B��B��B��B��B�B�LBĜBĜBĜB��B�jB�jB�qB��BĜBȴB��B��B�#B�#B�;B�B�B�B�B�B��B��B��B��B	  B	B	B	%B	%B	B	%B	
=B	\B	DB		7B	+B	B	B	B	JB	JB	PB	hB	�B	�B	�B	{B	�B	�B	�B	�B	�B	�B	 �B	#�B	'�B	(�B	,B	/B	2-B	6FB	7LB	9XB	>wB	?}B	?}B	E�B	H�B	I�B	I�B	I�B	K�B	O�B	YB	]/B	_;B	aHB	dZB	gmB	hsB	m�B	q�B	r�B	t�B	u�B	v�B	x�B	z�B	|�B	�B	�1B	�=B	�JB	�PB	�PB	�JB	�JB	�PB	�VB	�PB	�7B	�=B	�DB	�\B	�\B	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�FB	�RB	�RB	�XB	�qB	�}B	��B	��B	��B	��B	��B	��B	��B	�}B	��B	��B	��B	ÖB	ŢB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;B	�;B	�BB	�HB	��B
3B
�B
�B
$&B
/ B
9�B
?�B
=�B
D�B
K�B
M�B
S@B
W
B
\�B
d�B
k6B
l�B
oiB
s�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�:B
�:B
�:B
�:B
�:B
�:B
�4B
�4B
�4B
�4B
�4B
�4B
�4B
�4B
�4B
�4B
�4B
�4B
�.B
�.B
�.B
�9B
�4B
�4B
�4B
�4B
�4B
�4B
�4B
�FB
�@B
�dB
�kB
�dB
�qB
ɂB
̕B
̕B
̕B
̕B
̕B
̕B
͛B
͛B
ΡB
͛B
ΡB
ΡB
ΡB
͛B
͛B
ΡB
ΡB
ΡB
ӿB
��B
��B'�BCSBf!B��B�xB�B�sB�aB�gB��B�MB�kB �B��B��B�BeB)�B"B rBfBfB[B7BBBB
�BB-B3BPBoBoB^BKB@B!B�B�B��B�sB�OB�BտBѦB˂B�XB�LB�(B��BocBjFBi@Bd!B]�BR�BJ�B.�B
��B
��B
�B
V�B
%�B
NB	�KB	�-B	ʴB	��B	��B	��B	�B	t�B	dZB	^6B	XB	M�B	B�B	/ B	 �B	�B	fB	`B	NB	<B	B��B�B�KB�B��B��B��BéB��B��B�tB�bB�%B��B��B�}B�dB�GB�#B�#B}By�Bz�Bt�Bs�Bq�Bp�Bn�Bl�Bj�Be�Be�B`dB_^B\LB[GBW.BV)BU#BTBU#BSBRBY;BbrBw�B�-B}Bw�Bp�Bj�BcyBY<Bk�Bg�Bg�Bn�Bx�B�:By�B|B�eB��B��B��B�AB�<B��B�IB{Bq�Bj�BB��B��B�B��B B|B��B�)B®B��B®B�xB� B{Bi�Bm�BT$BJ�BQBT$BX<Be�Bx�B~B�@B�SB#B�.B{
Bl�Bf�B_gBYCBU,BU,BV2BV2B^bB]\BZJBT'BS BJ�BL�BQBQBRBU.Be�Bo�Bo�Bj�Bl�Br�B}Bu�Bn�Bk�Bw�B~ B|B~ B{Bt�Bp�Bo�Br�Br�Bs�B|B}B~!B~"B�/B)B)B�AB�_B�wB�~B��B��B��B��B��B��B��B�-B�|B�|B�|B�dB�KB�LB�RB�dB�}B��B��B��B�B�B�B�\B�nB�uB�{B�B�B�B�B�B��B��B��B� B� B��B�B�B	7B�B�B�B��B��B��B	 %B	 &B	+B	CB	�B	�B	�B	VB	uB	uB	uB	uB	{B	�B	�B	�B	�B	�B	�B	"�B	&B	*B	+$B	-0B	2OB	3UB	3UB	9yB	<�B	=�B	=�B	=�B	?�B	C�B	L�B	QB	SB	UB	X.B	[AB	\GB	aeB	e}B	f�B	h�B	i�B	j�B	l�B	n�B	p�B	v�B	|B	~B	�B	�!B	�!B	�B	�B	�!B	�'B	�!B	}	B	~B	B	�-B	�-B	�3B	�9B	�FB	�RB	�XB	�cB	�cB	�jB	�pB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	� B	�&B	�>B	�JB	�PB	�PB	�PB	�PB	�PB	�PB	�PB	�JB	�PB	�VB	�VB	�cB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	ëB	ŸB	ŸB	ƾB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�G�O�B	�B	��B
�B
~B
�B
"�B
-QB
3�B
1�B
8aB
?�B
A~B
GB
J�B
P�B
X�B
^�B
`NB
c,B
g�B
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.19 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9997(+/-0.0001), vertically averaged dS =-0.012(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144232022020411442320220204114423  AO  ARCAADJP                                                                    20200619170924    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200619170924  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200619170924  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114423  IP                  G�O�G�O�G�O�                