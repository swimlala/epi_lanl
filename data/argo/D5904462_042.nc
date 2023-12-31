CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-03-23T09:16:40Z AOML 3.0 creation; 2016-08-07T21:51:16Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150323091640  20160807145116  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               *A   AO  5287_9017_042                   2C  D   APEX                            6529                            072314                          846 @�C��T2
1   @�C����@1������d�bM��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    *A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C�fC  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy��D�fD�C3D���D�� D�	�D�6fD�p D��fD�	�D�@ D��fD��fD�	�D�P D�vfD��fD��D�9�D�s3D�|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @H��@��@ǮA�
A#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��B�z�B��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC=qC=qC
WC=qC#�C=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5�D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt��Dy�)D�D�J�D��{D�׮D�HD�>D�w�D��D�HD�G�D��D��D�HD�W�D�~D��D�{D�AHD�z�D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�O�A�A�A�JA�ĜA�dZA�9XA�+A��A�{A�bA�
=A�A���A���A���A���A���A���A��A��A��A��A��mA���Aʗ�Aʇ+A�v�A�S�A�E�A�A��
A�ƨA���A���A��yA��
AɮA�`BA�$�A��#Aȟ�A��Aƕ�A�9XA�`BA��uA�z�A��A�-A�33A���A��+A�jA��!A�;dA�t�A�7LA�p�A�M�A�+A�ƨA���A�`BA�x�A��RA���A��9A��FA�$�A��#A��A�~�A��A�=qA�K�A���A���A�t�A�?}A�%A���A���A��uA���A�ȴA�&�A���A��A�hsA�ZA�+A�E�A���A��A��PA�bNA���A��jA��`A���A��A�bNA���A�|�A���A�ĜA~~�Awt�AwoAu��Au\)As��AqK�Ao�Anv�Amp�AljAjE�AgK�Ae��Ad1'Ab(�AaC�A`��A_33AZ�HAY�hAY/AYoAX~�AT��AQO�APZANE�AK�AI`BAG�wAFVAD�/ABz�AAA>��A=�A;hsA8��A7&�A6bA4��A3�A2^5A1"�A0{A/�FA.�!A-VA*�A(ĜA'oA&v�A$�A#�FA"�uA"A!hsA =qA�PA�Az�A�A�A�hA��AA��A �A`BA�A=qA�TA�hAXA�A��Az�A7LA^5A��A%A�+AhsA�AA?}A
�A	�mA	�hA	�7A	`BA	�A�uA1A�^A&�A  A��A�A��A=qAbA�A?}A r�A M�A (�@���@�r�@�S�@��@��H@�5?@���@�bN@��;@�ȴ@�7L@��`@�\)@�z�@�+@�9@�z�@�  @��H@��#@�|�@�{@���@�r�@�(�@�w@�+@◍@�n�@�V@�=q@���@��@��@�  @�
=@�$�@�p�@���@ܣ�@ܛ�@�Z@�A�@�9X@�(�@� �@�  @ۥ�@�5?@��@�1@�ȴ@և+@�-@�X@�r�@���@���@�z�@�X@�(�@���@Гu@���@�@�$�@��@�^5@�9X@�M�@�p�@�Ĝ@���@Ǯ@�1'@�9X@Ɨ�@�5?@�n�@��@Ə\@�^5@�X@��@�A�@�\)@���@���@�l�@�;d@���@���@�v�@��\@���@�ȴ@��y@���@�~�@�=q@���@���@�?}@��@��@�(�@�9X@�  @�C�@�5?@�r�@��@�Z@�(�@��@�t�@�\)@��@�{@���@�?}@���@��9@��u@�bN@�1@���@���@�@��@�E�@��@��h@���@��u@��w@��P@�33@�ȴ@�-@��-@�G�@�7L@�V@�Ĝ@���@���@� �@��F@�dZ@�33@�"�@�
=@��y@���@�M�@�@���@��h@�&�@���@��j@��@�bN@�I�@�  @�\)@�+@�;d@��@�@�+@���@�=q@��-@�X@��@���@���@��/@��`@�Q�@�ƨ@�l�@�S�@���@��!@�~�@�$�@���@���@���@�hs@�O�@��@��9@�A�@��F@��P@�\)@�;d@�"�@��@���@���@�V@��@��@�`B@�&�@��@���@�j@�Q�@�1@���@��P@��@�dZ@�K�@�"�@�^5@��@���@�p�@�?}@��@��@��/@���@�z�@�bN@�I�@�9X@� �@��w@�|�@��H@�n�@�E�@�@��#@���@��@�7L@�7L@��@��@���@��9@�z�@�Q�@�9X@�1@���@�"�@�@���@��+@�@��#@���@��^@���@���@���@�x�@�X@�?}@��@���@�Ĝ@���@�r�@�1'@�hs@�p�@~E�@t�j@m@fff@]V@V{@MO�@Gl�@BM�@:~�@3C�@,�@)7L@"�@��@hs@��@Q�@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�O�A�A�A�JA�ĜA�dZA�9XA�+A��A�{A�bA�
=A�A���A���A���A���A���A���A��A��A��A��A��mA���Aʗ�Aʇ+A�v�A�S�A�E�A�A��
A�ƨA���A���A��yA��
AɮA�`BA�$�A��#Aȟ�A��Aƕ�A�9XA�`BA��uA�z�A��A�-A�33A���A��+A�jA��!A�;dA�t�A�7LA�p�A�M�A�+A�ƨA���A�`BA�x�A��RA���A��9A��FA�$�A��#A��A�~�A��A�=qA�K�A���A���A�t�A�?}A�%A���A���A��uA���A�ȴA�&�A���A��A�hsA�ZA�+A�E�A���A��A��PA�bNA���A��jA��`A���A��A�bNA���A�|�A���A�ĜA~~�Awt�AwoAu��Au\)As��AqK�Ao�Anv�Amp�AljAjE�AgK�Ae��Ad1'Ab(�AaC�A`��A_33AZ�HAY�hAY/AYoAX~�AT��AQO�APZANE�AK�AI`BAG�wAFVAD�/ABz�AAA>��A=�A;hsA8��A7&�A6bA4��A3�A2^5A1"�A0{A/�FA.�!A-VA*�A(ĜA'oA&v�A$�A#�FA"�uA"A!hsA =qA�PA�Az�A�A�A�hA��AA��A �A`BA�A=qA�TA�hAXA�A��Az�A7LA^5A��A%A�+AhsA�AA?}A
�A	�mA	�hA	�7A	`BA	�A�uA1A�^A&�A  A��A�A��A=qAbA�A?}A r�A M�A (�@���@�r�@�S�@��@��H@�5?@���@�bN@��;@�ȴ@�7L@��`@�\)@�z�@�+@�9@�z�@�  @��H@��#@�|�@�{@���@�r�@�(�@�w@�+@◍@�n�@�V@�=q@���@��@��@�  @�
=@�$�@�p�@���@ܣ�@ܛ�@�Z@�A�@�9X@�(�@� �@�  @ۥ�@�5?@��@�1@�ȴ@և+@�-@�X@�r�@���@���@�z�@�X@�(�@���@Гu@���@�@�$�@��@�^5@�9X@�M�@�p�@�Ĝ@���@Ǯ@�1'@�9X@Ɨ�@�5?@�n�@��@Ə\@�^5@�X@��@�A�@�\)@���@���@�l�@�;d@���@���@�v�@��\@���@�ȴ@��y@���@�~�@�=q@���@���@�?}@��@��@�(�@�9X@�  @�C�@�5?@�r�@��@�Z@�(�@��@�t�@�\)@��@�{@���@�?}@���@��9@��u@�bN@�1@���@���@�@��@�E�@��@��h@���@��u@��w@��P@�33@�ȴ@�-@��-@�G�@�7L@�V@�Ĝ@���@���@� �@��F@�dZ@�33@�"�@�
=@��y@���@�M�@�@���@��h@�&�@���@��j@��@�bN@�I�@�  @�\)@�+@�;d@��@�@�+@���@�=q@��-@�X@��@���@���@��/@��`@�Q�@�ƨ@�l�@�S�@���@��!@�~�@�$�@���@���@���@�hs@�O�@��@��9@�A�@��F@��P@�\)@�;d@�"�@��@���@���@�V@��@��@�`B@�&�@��@���@�j@�Q�@�1@���@��P@��@�dZ@�K�@�"�@�^5@��@���@�p�@�?}@��@��@��/@���@�z�@�bN@�I�@�9X@� �@��w@�|�@��H@�n�@�E�@�@��#@���@��@�7L@�7L@��@��@���@��9@�z�@�Q�@�9X@�1@���@�"�@�@���@��+@�@��#@���@��^@���@���@���@�x�@�X@�?}@��@���@�Ĝ@���@�r�G�O�@�hs@�p�@~E�@t�j@m@fff@]V@V{@MO�@Gl�@BM�@:~�@3C�@,�@)7L@"�@��@hs@��@Q�@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
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
�B
�fB
�B
�B
��B+B�B&�B33BL�Bw�B�7B�!B�)B��B �B49B>wBL�BffBjBp�Bw�B� B�JB��B�LB�XB�jB�wBŢBĜBĜB��B�B�VB�+Br�BffB^5BYBYB]/BaHBVBN�BE�B2-B'�B(�B��B�fB  BPB	7BB  B��B�mB��B��B�LB�{B�B� B�+Bw�BaHBD�B8RB(�BhB  B
��B
��B
�5B
ȴB
�B
m�B
`BB
YB
I�B
'�B
B	�B	��B	��B	ǮB	�qB	�B	��B	��B	��B	�hB	�B	t�B	k�B	dZB	ZB	VB	R�B	H�B	5?B	/B	-B	,B	&�B	�B	�B	�B	�B	\B	1B	B��B��B�B�B�TB�5B�B��B��B��BǮBŢBƨB��B�;B�HB�;B�NB�mB�NB�#B�B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B�#B�/B�;B�BB�TB�TB�TB�TB�ZB�ZB�ZB�sB�B�B�B��B��B��B	B	B	B	JB	VB	\B	bB	hB	bB	bB	hB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	�B	�B	)�B	+B	)�B	+B	&�B	&�B	&�B	$�B	$�B	$�B	 �B	�B	�B	�B	�B	!�B	�B	�B	%�B	&�B	(�B	.B	33B	5?B	7LB	9XB	:^B	:^B	:^B	:^B	9XB	9XB	9XB	<jB	?}B	B�B	B�B	B�B	D�B	E�B	F�B	F�B	F�B	F�B	G�B	M�B	O�B	O�B	N�B	N�B	M�B	N�B	N�B	M�B	P�B	ZB	m�B	m�B	cTB	dZB	n�B	w�B	v�B	q�B	jB	bNB	\)B	`BB	aHB	`BB	dZB	l�B	n�B	o�B	o�B	q�B	r�B	w�B	|�B	z�B	y�B	x�B	v�B	p�B	l�B	k�B	l�B	l�B	m�B	n�B	v�B	|�B	� B	�B	�B	�B	�B	�B	�B	�=B	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�3B	�9B	�?B	�RB	�XB	�XB	�dB	�dB	�jB	�dB	�jB	�qB	�wB	�wB	�}B	��B	��B	B	ÖB	ÖB	ĜB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�/B	�)B	�;B	�NB	�NB	�TB	�`B	�`B	�TB	�TB	�ZB	�`B	�fB	�yB	�B	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
	7B
�B
 �B
'�B
)�B
0!B
7LB
<jB
@�B
D�B
H�B
Q�B
]/B
bNB
ffB
iyB
m�B
o�B
s�B
x�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
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
�B
�TB
�B
�B
��BB�B&�B3!BL�Bw�B�#B�B�B��B �B4$B>bBL�BfSBjnBp�Bw�B�B�6B��B�7B�EB�VB�hBŌBĊBćB�qB�B�CB�Br�BfRB^BYBYB]Ba1BU�BN�BE�B2B'�B(�B��B�PB��B7B	BB��B��B�VB��BʬB�4B�dB�B�B�Bw�Ba1BD�B8<B(�BRB
��B
��B
��B
�!B
ȟB
��B
mB
`1B
YB
I�B
'�B
B	��B	��B	̼B	ǟB	�dB	�B	��B	��B	��B	�[B	�B	t�B	kzB	dOB	ZB	U�B	R�B	H�B	56B	/B	-B	+�B	&�B	�B	�B	�B	vB	TB	)B	 B��B��B�B�xB�MB�/B�B��B��BʸBǦBŘBƝB��B�1B�>B�3B�EB�cB�CB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�#B�0B�8B�GB�HB�JB�JB�OB�PB�OB�gB�uB�B�B��B��B��B	 �B	�B	B	>B	GB	OB	TB	ZB	WB	SB	YB	bB	zB	xB	�B	~B	B	xB	�B	�B	�B	�B	 �B	 �B	�B	�B	)�B	*�B	)�B	*�B	&�B	&�B	&�B	$�B	$�B	$�B	 �B	�B	�B	�B	�B	!�B	�B	�B	%�B	&�B	(�B	.B	3!B	50B	7:B	9HB	:NB	:MB	:LB	:MB	9DB	9EB	9GB	<YB	?lB	B}B	BB	B~B	D�B	E�B	F�B	F�B	F�B	F�B	G�B	M�B	O�B	O�B	N�B	N�B	M�B	N�B	N�B	M�B	P�B	ZB	mB	m|B	cCB	dFB	n�B	w�B	v�B	q�B	jmB	b9B	\B	`.B	a5B	`.B	dGB	lxB	n�B	o�B	o�B	q�B	r�B	w�B	|�B	z�B	y�B	x�B	v�B	p�B	lwB	krB	lxB	lwB	m}B	n�B	v�B	|�B	�B	��B	��B	�B	�B	�B	�B	�)B	�BB	�MB	�^B	�jB	�qB	��B	��B	�jB	�mB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	� B	�%B	�)B	�:B	�BB	�?B	�MB	�MB	�RB	�MB	�RB	�ZB	�aB	�bB	�eB	�lB	�nB	�zB	�~B	�~B	ĄB	ŋB	ǘB	ʪB	̶B	ͺB	ͺB	��B	��B	��B	��B	��B	�B	�B	�B	�%B	�4B	�7B	�<B	�JB	�HB	�<B	�;B	�BB	�GB	�PB	�aB	�hB	�hB	�aB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
B
�B
B
B
B
B
G�O�B
	B
sB
 �B
'�B
)�B
0B
70B
<QB
@jB
D�B
H�B
Q�B
]B
b4B
fMB
iaB
mvB
o�B
s�B
x�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.24 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451162016080714511620160807145116  AO  ARCAADJP                                                                    20150323091640    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150323091640  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150323091640  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145116  IP                  G�O�G�O�G�O�                