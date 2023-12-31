CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-02-01T01:01:26Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20180201010126  20190604094144  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4051                            2C  D   APEX                            5368                            041511                          846 @�H�:��k1   @�H��o��@4��vȴ9�d}�hr�!1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�DyZ=D�D�I�D�y�D���D��D�A�D�w
D�߮D�
D�F�D��fD�ؤD�
=D�Q�Dڄ)D�ȤD��D�qD�g
D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @:�H@���@���A Q�A Q�A@Q�A`Q�A�(�A�(�A�(�A�(�A�(�A�(�A�(�A�(�B {B{B{B{B {B({B0{B8{B@{BH{BP{BX{B`{Bh{Bp{Bx{B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B�
=B��
B�
=B�
=B�
=B�
=B�
=B�
=C CCCCC
CCCCCCCC�CCC C"C$C&C(C*C,C.C0C2C4C6C8C:C<C>C@CBCDCFCHCJCLCNCPCRCTCVCXCZC\C^C`CbCdCfChCjClCnCpCrCtCvCxCzC|C~C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D HD �HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD	HD	�HD
HD
�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HDHD�HD HD �HD!HD!�HD"HD"�HD#HD#�HD$HD$�HD%HD%�HD&HD&�HD'HD'�HD(HD(�HD)HD)�HD*HD*�HD+HD+�HD,HD,�HD-HD-�HD.HD.�HD/HD/�HD0HD0�HD1HD1�HD2HD2�HD3HD3�HD4HD4�HD5HD5�HD6HD6�HD7HD7�HD8HD8�HD9HD9�HD:HD:�HD;HD;�HD<HD<�HD=HD=�HD>HD>�HD?HD?�HD@HD@�HDAHDA�HDBHDB�HDCHDC�HDDHDD�HDEHDE�HDFHDF�HDGHDG�HDHHDH�HDIHDI�HDJHDJ�HDKHDK�HDLHDL�HDMHDM�HDNHDN�HDOHDO�HDPHDP�HDQHDQ�HDRHDR�HDSHDS�HDTHDT�HDUHDU�HDVHDV�HDWHDW�HDXHDX�HDYHDY�HDZHDZ�HD[HD[�HD\HD\�HD]HD]�HD^HD^�HD_HD_�HD`HD`�HDaHDa�HDbHDb�HDcHDc�HDdHDd�HDeHDe�HDfHDf�HDgHDg�HDhHDh�HDiHDi�HDjHDj�HDkHDk�HDlHDl�HDmHDm�HDnHDn�HDoHDo�HDpHDp�HDqHDq�HDrHDr�HDsHDs�HDtHDtz�Dy[�D��D�J>D�z>D��HD�)D�B�D�w�D��RD��D�G\D��
D��HD�
�D�R�Dڄ�D��HD�{D�D�g�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�;dA�+A� �A��A��A��A��A�oA�1A�
=A�%A�%A�%A�A�%A�%A�A�A�A�A�A�A�%A��A���AȮAș�AȍPA�n�A�A�A�"�A�
=A��`A���Aǲ-AǬAǗ�A�1'A��A���AƅA��A���A�ZAģ�A�A�A�ĜAÁA�Q�A�M�A�ĜA�=qA�33A��A��A�ȴA�p�A�-A��yA���A���A��+A�A���A���A�=qA��jA�7LA�`BA��A�r�A��A�dZA��A���A���A��A�K�A���A��uA��PA�\)A���A�O�A��HA�K�A�$�A�=qA��RA���A�1'A�^5A�$�A��A�E�A��A��RA��DA��A��;A�t�A�n�A�r�A�O�A�M�A���A�ZA�?}A�1'A���A���A���A�1A�?}A�1'A�33A��/A��`A�A�Q�A�x�A��A{�TAzr�Ax�Aw�wAv�\At�AsC�Ar�+ArffAr5?AqƨAp��An��AlI�Ai�wAf�9Ae�mAd5?Ab��AbM�A_��A\�A\ZA[�A[AY�AY"�AX=qAV��AT��AS��ASoAQ��AO��AM��AMdZAK�^AI�-AHAF��AE+AC�AA�A@��A@��A?t�A>��A=��A;�A:��A8�9A69XA3�A1�
A0ȴA.1'A,JA)��A(1'A'��A&1'A#�-A#A"{A ��AXAz�A�HA��A�Az�A�hA{A��AĜA�+A��A��AS�AȴA1A�FA��A�hA  Ax�AA�AO�A
��A
$�A	��A	C�A�/A�+AA"�AA�/Az�Al�A~�A��A��@�ƨ@�;d@��/@�Z@��;@�C�@�~�@���@�@���@�u@�F@�K�@��@�^5@���@�7L@�1'@��y@���@�x�@�@�!@��@�P@�-@��@��@�1@�;d@ޗ�@��@�X@ܴ9@��;@���@�@� �@ְ!@�X@�r�@�t�@�ȴ@щ7@��@д9@�1@ϕ�@�S�@Χ�@͑h@̋D@˅@�E�@��@��T@�7L@��@�S�@��@���@�%@ģ�@���@�;d@�
=@�^5@��h@��`@�b@��\@��^@��h@�G�@��@�%@��/@��u@�9X@�S�@���@�@��j@�A�@��F@��+@���@��h@�A�@�\)@��R@�V@�-@���@��-@�hs@�/@�Z@���@�@�ȴ@���@��@��u@���@�;d@��@���@�M�@�M�@�V@��@���@���@��7@�O�@��@�Ĝ@��@�Z@� �@��@��P@�\)@��@�ȴ@���@��\@�ff@�@��7@�G�@�&�@��@��/@�Z@��m@�ƨ@�\)@��H@��+@�~�@�v�@�=q@�{@���@�p�@�p�@���@��u@��@���@�K�@�
=@�v�@�5?@���@���@��7@�G�@�/@�&�@���@�Ĝ@�Z@��w@�C�@�C�@��H@���@�-@���@�p�@�&�@��@���@�j@� �@�b@�1@�1@��
@���@�|�@�o@��!@�n�@�=q@�J@��@��@���@���@�x�@�7L@�/@�/@�?}@�&�@�%@���@�Z@�  @���@���@���@���@��@���@�C�@�+@�o@��H@���@�~�@�v�@�J@���@���@�O�@��@��/@��9@���@�Z@��;@�C�@��!@��\@�v�@��@�{@��@�{@��#@��-@�X@��j@�bN@� �@��m@���@��w@�|�@�+@��@��@�@��R@��!@��@���@��\@�ff@�$�@��#@��@���@��^@�Ĝ@�Z@�I�@�A�@��5@�u�@{g�@toi@lPH@c(@XɆ@P[�@G˒@ArG@<Ɇ@5o @.YK@(�@$A�@
=@��@��@;d@��@ѷ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�A�A�;dA�+A� �A��A��A��A��A�oA�1A�
=A�%A�%A�%A�A�%A�%A�A�A�A�A�A�A�%A��A���AȮAș�AȍPA�n�A�A�A�"�A�
=A��`A���Aǲ-AǬAǗ�A�1'A��A���AƅA��A���A�ZAģ�A�A�A�ĜAÁA�Q�A�M�A�ĜA�=qA�33A��A��A�ȴA�p�A�-A��yA���A���A��+A�A���A���A�=qA��jA�7LA�`BA��A�r�A��A�dZA��A���A���A��A�K�A���A��uA��PA�\)A���A�O�A��HA�K�A�$�A�=qA��RA���A�1'A�^5A�$�A��A�E�A��A��RA��DA��A��;A�t�A�n�A�r�A�O�A�M�A���A�ZA�?}A�1'A���A���A���A�1A�?}A�1'A�33A��/A��`A�A�Q�A�x�A��A{�TAzr�Ax�Aw�wAv�\At�AsC�Ar�+ArffAr5?AqƨAp��An��AlI�Ai�wAf�9Ae�mAd5?Ab��AbM�A_��A\�A\ZA[�A[AY�AY"�AX=qAV��AT��AS��ASoAQ��AO��AM��AMdZAK�^AI�-AHAF��AE+AC�AA�A@��A@��A?t�A>��A=��A;�A:��A8�9A69XA3�A1�
A0ȴA.1'A,JA)��A(1'A'��A&1'A#�-A#A"{A ��AXAz�A�HA��A�Az�A�hA{A��AĜA�+A��A��AS�AȴA1A�FA��A�hA  Ax�AA�AO�A
��A
$�A	��A	C�A�/A�+AA"�AA�/Az�Al�A~�A��A��@�ƨ@�;d@��/@�Z@��;@�C�@�~�@���@�@���@�u@�F@�K�@��@�^5@���@�7L@�1'@��y@���@�x�@�@�!@��@�P@�-@��@��@�1@�;d@ޗ�@��@�X@ܴ9@��;@���@�@� �@ְ!@�X@�r�@�t�@�ȴ@щ7@��@д9@�1@ϕ�@�S�@Χ�@͑h@̋D@˅@�E�@��@��T@�7L@��@�S�@��@���@�%@ģ�@���@�;d@�
=@�^5@��h@��`@�b@��\@��^@��h@�G�@��@�%@��/@��u@�9X@�S�@���@�@��j@�A�@��F@��+@���@��h@�A�@�\)@��R@�V@�-@���@��-@�hs@�/@�Z@���@�@�ȴ@���@��@��u@���@�;d@��@���@�M�@�M�@�V@��@���@���@��7@�O�@��@�Ĝ@��@�Z@� �@��@��P@�\)@��@�ȴ@���@��\@�ff@�@��7@�G�@�&�@��@��/@�Z@��m@�ƨ@�\)@��H@��+@�~�@�v�@�=q@�{@���@�p�@�p�@���@��u@��@���@�K�@�
=@�v�@�5?@���@���@��7@�G�@�/@�&�@���@�Ĝ@�Z@��w@�C�@�C�@��H@���@�-@���@�p�@�&�@��@���@�j@� �@�b@�1@�1@��
@���@�|�@�o@��!@�n�@�=q@�J@��@��@���@���@�x�@�7L@�/@�/@�?}@�&�@�%@���@�Z@�  @���@���@���@���@��@���@�C�@�+@�o@��H@���@�~�@�v�@�J@���@���@�O�@��@��/@��9@���@�Z@��;@�C�@��!@��\@�v�@��@�{@��@�{@��#@��-@�X@��j@�bN@� �@��m@���@��w@�|�@�+@��@��@�@��R@��!@��@���@��\@�ff@�$�@��#@��@���@��^@�Ĝ@�Z@�I�G�O�@��5@�u�@{g�@toi@lPH@c(@XɆ@P[�@G˒@ArG@<Ɇ@5o @.YK@(�@$A�@
=@��@��@;d@��@ѷ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBm�Bm�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bm�Bm�Bo�Bs�B{�B��B��B��B�B�
B�B�)B�BB�B��B	7B
=B
=BJB�B �B"�B/B49B;dBE�BB�B;dB7LBR�BW
BcTB`BBYBJ�B=qB0!B8RB?}B;dB7LB2-B-B0!B49B8RBB�BF�BiyB}�B�=B�7B�+B�B�B|�Bu�Bp�Bm�BjBe`BW
BF�B;dB/B(�B#�B�BbB%B  B��B�B�fB��BƨB�9B��B��B�DBw�Bk�BdZBYBJ�B<jB1'B+B"�B�B
=BBB
��B
�B
�ZB
�}B
�B
��B
��B
�7B
|�B
k�B
O�B
7LB
<jB
C�B
:^B
5?B
)�B
#�B
�B
�B
�B
�B
oB
%B	��B	�B	��B	��B	��B	�qB	�XB	�B	��B	��B	��B	��B	�uB	�VB	�7B	�B	v�B	o�B	jB	e`B	[#B	O�B	K�B	D�B	;dB	2-B	)�B	 �B	�B	PB	DB		7B	B	B��B��B�B�fB�#B��BB�jB�?B�B�B��B��B��B��B�{B�bB�PB�DB�1B�B�B� B{�Bx�Bv�Bt�Bt�Bu�Bu�Bt�Br�Bo�Bm�Bk�BiyBgmBe`Be`BdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBe`BdZBgmBl�Bl�BjBjBl�Bo�Bo�Bo�Bo�Bo�Bn�Bo�Bo�Bp�Bp�Bo�Bo�Bo�Bo�Bo�Bo�Bn�Bn�Bm�Bm�Bm�Bn�Bm�Bm�Bo�Bo�Bn�Bo�Bo�Bo�Bo�Bo�Bo�Bp�Bq�Bt�Bv�Bx�Bz�B|�B}�B� B�B�B�B�B�B�B�+B�=B�JB�\B�\B�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B�'B�FB�FB�LB�LB�RB�RB�XB�^B�wBBƨB��B��B��B�B�)B�/B�TB�mB�B�B�B��B��B��B��B��B	B	B	B	+B	PB	uB	�B	�B	�B	!�B	%�B	%�B	%�B	'�B	.B	.B	/B	1'B	33B	5?B	6FB	6FB	8RB	9XB	<jB	@�B	C�B	F�B	H�B	H�B	I�B	L�B	O�B	Q�B	R�B	VB	ZB	\)B	^5B	_;B	aHB	dZB	e`B	ffB	ffB	hsB	jB	m�B	p�B	s�B	t�B	u�B	w�B	y�B	{�B	|�B	�B	�B	�%B	�1B	�=B	�VB	�VB	�bB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�?B	�LB	�RB	�^B	�jB	�qB	�wB	�}B	��B	��B	��B	B	ÖB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�#B	�5B	�;B	�;B	�;B	�;B	�;B	�;B	�BB	�BB	�NB	�ZB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B

=B
�B
OB
'8B
2�B
:xB
?}B
C�B
L0B
RB
T�B
[qB
abB
f�B
jB
oiB
s3B
vzB
x�B
}�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B__B__B^WB^UB^SB^SB^SB^SB^SB^UB^UB^UB^WB^SB^SB^UB^UB^UB^SB_[B_]BaiBe�Bm�B��B��BĲB��B��B��B��B�B�<B�B��B��B��B�B	FB|B�B �B%�B-B7SB4HB-B) BD�BH�BUBQ�BJ�B<vB/&B!�B*B12B-B)B#�B�B!�B%�B*B4FB8^B[+Bo�B{�Bz�Bx�Bv�Bs�Bn�BgzBbWB_FB\5BWBH�B8bB-"B �B�B�BgB$B��B��B�B�nB�0B��B�rB�B��B�dB}Bi�B][BV1BJ�B<�B.IB#B�B�BdB
�B
�B
��B
��B
�B
�AB
�fB
�	B
��B
��B
{&B
n�B
]{B
A�B
)IB
.lB
5�B
,\B
'<B
�B
�B
�B
�B
�B

�B
rB	�)B	��B	܇B	�B	��B	��B	��B	�gB	�$B	��B	��B	��B	��B	��B	�fB	{KB	t!B	h�B	a�B	\�B	WxB	M@B	A�B	=�B	6�B	-�B	$MB	!B	�B	
�B�wB�kB�[B�9B�/B�B��B��BؐB�QB��B��B��B�pB�NB�3B�B�B��B��B��B��B�B}}BziBwYBuKBr:Bn!BkBh�Bf�Bf�Bh BhBf�Bd�Ba�B_�B]�B[�BY�BW�BW�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BW�BV�BY�B^�B^�B\�B\�B^�Ba�Ba�Ba�Ba�Ba�B`�Ba�Ba�Bb�Bb�Ba�Ba�Ba�Ba�Ba�Ba�B`�B`�B_�B_�B_�B`�B_�B_�Ba�Ba�B`�Ba�Ba�Ba�Ba�Ba�Ba�Bb�Bc�Bg BiBkBm%Bo2Bp8BrDBsJBsIBtMBuUBtPBv_BymB|�B~�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�,B�AB�gB��B��B��B��B��B��B��B��B��B��B��B�B�B�+B�SB�fB�jBՌB٩B��B��B��B��B�B�B�B�/B�?B�LB�WB�eB��B	�B		�B	�B	�B	B	B	B	B	&B	 KB	 IB	!PB	#^B	%iB	'uB	(yB	(|B	*�B	+�B	.�B	2�B	5�B	8�B	:�B	:�B	;�B	?B	BB	D!B	E!B	H5B	LJB	NYB	PgB	QmB	SyB	V�B	W�B	X�B	X�B	Z�B	\�B	_�B	b�B	e�B	f�B	g�B	i�B	l
B	nB	oB	t6B	vHB	xRB	z_B	|gB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�!B	�&B	�8B	�8B	�8B	�:B	�?B	�KB	�QB	�ZB	�gB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�0B	�7B	�=B	�;B	�;B	�IB	�XB	�aB	�`B	�cB	�_B	�_B	�bB	�fB	�hB	�uB	�|B	ٓB	۞B	۟B	ܞB	ܣB	ܥB	ܣB	ܤB	ްB	ޱB	ޭB	ߵB	�B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�"B	� B	�%G�O�B	�]B

�B
nB
XB
%B
,�B
1�B
6B
>OB
D$B
GB
M�B
S�B
X�B
\�B
a�B
eNB
h�B
kB
o�B
s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =-0.02 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            r =0.9996(+/-0.0001), vertically averaged dS =-0.014(+/-0.002) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Map scales: x=6,3; y=2,1. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                            201906040941442019060409414420190604094144  AO  ARCAADJP                                                                    20180201010126    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180201010126  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180201010126  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604094144  IP                  G�O�G�O�G�O�                