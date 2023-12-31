CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-06T10:16:58Z AOML 3.0 creation; 2016-08-07T21:51:24Z UW 3.1 conversion     
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
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151206101658  20160807145124  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               [A   AO  5287_9017_091                   2C  D   APEX                            6529                            072314                          846 @ׄf�?1   @ׄ`�a@0Ƨ-�d�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    [A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33Bș�B˙�B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy��D�fD�6fD�|�D���D���D�P D���D���D��D�33D��3D�ɚD� D�&fDړ3D�ɚD�	�D�33D�|�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��G@ǮA�
A#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��B�z�B�z�B�z�B�z�BĮB�{B�{B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%�D%��D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dtu�Dy��D�D�>D��{D��{D�{D�W�D��{D��{D��HD�:�D���D��HD��D�.Dښ�D��HD�HD�:�D�{D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�x�A�t�A�l�A�r�A�t�A�v�A�x�A�z�A�~�A؃A؃A؃A؍PAؓuAؗ�Aؗ�Aؕ�Aؗ�Aؗ�A؛�A؛�AؼjA���A��A�AفA��mA�n�A�ƨA���A���AڼjAں^AڶFAڸRA���A��#A��#A�ĜAڡ�A�C�AٶFA�I�A���AԮA�^5A���A�E�A�JA�v�A���Aɲ-A�XAĮA��A¡�A���A���A�Q�A���A�XA�oA��A�?}A�l�A�bNA�JA���A�`BA��A�ȴA��^A���A�\)A�~�A�r�A�A�ƨA�~�A�ZA�1A��hA�1'A��+A�  A�A�A�~�A��/A��#A�|�A��A�-A�hsA��\A��A�S�A���A��!A�bNA��A�^5A���A���A���A��uA��DA���A�VA�7LA�|�A��A���A}&�Az^5AwoAu�
AsdZAkO�Ah9XAel�Ab�jA`�jA`jA_��A^n�A\Q�AY7LAX �AW�TAW��AS��AP��AO�;AL�9AH��AFȴAE��AB��A=�A9�FA6ZA533A4��A2��A17LA0ffA/�A/oA-|�A,��A+�TA+G�A)�TA)�A(�RA'�mA'VA&�A%l�A%7LA$  A!�AG�AI�AA7LA^5AK�A�A(�A��AoA�A��A
bNA=qA�A$�A�;A�7A�A%AXAffA�;A��AbNAĜA	�An�A��AdZA��A��A-A�^A�A�
AC�A�\Ar�A^5A9XA1'A�;A|�AC�A/A �yA�AdZA��A��AbNAv�AbA�
AS�A%A �@�v�@��7@���@�Ĝ@��
@���@�z�@�z�@��P@�C�@��+@�~�@�^5@���@���@��7@��m@�@�V@�w@�~�@홚@�$�@�w@���@���@�?}@��@�@�j@�+@���@�7L@�%@���@�z�@�b@�C�@�
=@�"�@��@��@�o@���@�~�@�`B@��@�D@�9@���@�u@��@�@�l�@�@�\@�5?@�J@�`B@�O�@��`@�|�@�@��H@⟾@���@�Z@��@��m@�+@�t�@��@�j@�9X@��@�33@�v�@�M�@պ^@��@�t�@���@�I�@��@�ff@�n�@���@���@ˍP@�v�@�V@��@���@�@ˍP@�ƨ@���@�5?@�v�@ɩ�@ȣ�@�Q�@�9X@ǶF@��#@��@�7L@�@�$�@��#@��@�Ĝ@�r�@�\)@��H@�@�l�@�ȴ@�V@�@��h@���@�(�@�l�@�S�@�S�@�"�@�@�ȴ@�v�@�5?@��@�%@� �@�ƨ@�S�@��@��@�33@�dZ@�|�@�ƨ@��
@��F@��9@��@�%@��@�1@��P@�\)@��@��y@��!@���@�~�@�{@���@�O�@�%@���@���@���@��@�bN@��w@�|�@��@�dZ@��R@���@���@�`B@���@�Ĝ@�j@��
@�\)@�~�@�@��-@���@�A�@�
=@���@�ȴ@��R@��@�x�@�&�@���@���@��u@� �@��w@��@�C�@��@���@�^5@�{@��@���@�p�@�&�@���@��9@�Z@� �@�b@��
@��@���@�|�@�l�@�C�@�ȴ@�ff@�5?@��#@��h@�O�@��@�j@�Q�@�(�@� �@�b@�t�@���@���@�^5@�5?@�J@���@��7@�%@���@��j@��9@���@��@�9X@�(�@�1@��F@�t�@�33@��y@��R@���@�=q@���@��7@�&�@���@�z�@�9X@�1@���@���@�|�@�\)@�K�@�+@��@��+@�{@�Q�@��@�j@|�@o+@d�@W�@Ol�@I��@D9X@>ff@7+@0A�@)7L@#o@�-@��@��@@\)@C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�x�A�t�A�l�A�r�A�t�A�v�A�x�A�z�A�~�A؃A؃A؃A؍PAؓuAؗ�Aؗ�Aؕ�Aؗ�Aؗ�A؛�A؛�AؼjA���A��A�AفA��mA�n�A�ƨA���A���AڼjAں^AڶFAڸRA���A��#A��#A�ĜAڡ�A�C�AٶFA�I�A���AԮA�^5A���A�E�A�JA�v�A���Aɲ-A�XAĮA��A¡�A���A���A�Q�A���A�XA�oA��A�?}A�l�A�bNA�JA���A�`BA��A�ȴA��^A���A�\)A�~�A�r�A�A�ƨA�~�A�ZA�1A��hA�1'A��+A�  A�A�A�~�A��/A��#A�|�A��A�-A�hsA��\A��A�S�A���A��!A�bNA��A�^5A���A���A���A��uA��DA���A�VA�7LA�|�A��A���A}&�Az^5AwoAu�
AsdZAkO�Ah9XAel�Ab�jA`�jA`jA_��A^n�A\Q�AY7LAX �AW�TAW��AS��AP��AO�;AL�9AH��AFȴAE��AB��A=�A9�FA6ZA533A4��A2��A17LA0ffA/�A/oA-|�A,��A+�TA+G�A)�TA)�A(�RA'�mA'VA&�A%l�A%7LA$  A!�AG�AI�AA7LA^5AK�A�A(�A��AoA�A��A
bNA=qA�A$�A�;A�7A�A%AXAffA�;A��AbNAĜA	�An�A��AdZA��A��A-A�^A�A�
AC�A�\Ar�A^5A9XA1'A�;A|�AC�A/A �yA�AdZA��A��AbNAv�AbA�
AS�A%A �@�v�@��7@���@�Ĝ@��
@���@�z�@�z�@��P@�C�@��+@�~�@�^5@���@���@��7@��m@�@�V@�w@�~�@홚@�$�@�w@���@���@�?}@��@�@�j@�+@���@�7L@�%@���@�z�@�b@�C�@�
=@�"�@��@��@�o@���@�~�@�`B@��@�D@�9@���@�u@��@�@�l�@�@�\@�5?@�J@�`B@�O�@��`@�|�@�@��H@⟾@���@�Z@��@��m@�+@�t�@��@�j@�9X@��@�33@�v�@�M�@պ^@��@�t�@���@�I�@��@�ff@�n�@���@���@ˍP@�v�@�V@��@���@�@ˍP@�ƨ@���@�5?@�v�@ɩ�@ȣ�@�Q�@�9X@ǶF@��#@��@�7L@�@�$�@��#@��@�Ĝ@�r�@�\)@��H@�@�l�@�ȴ@�V@�@��h@���@�(�@�l�@�S�@�S�@�"�@�@�ȴ@�v�@�5?@��@�%@� �@�ƨ@�S�@��@��@�33@�dZ@�|�@�ƨ@��
@��F@��9@��@�%@��@�1@��P@�\)@��@��y@��!@���@�~�@�{@���@�O�@�%@���@���@���@��@�bN@��w@�|�@��@�dZ@��R@���@���@�`B@���@�Ĝ@�j@��
@�\)@�~�@�@��-@���@�A�@�
=@���@�ȴ@��R@��@�x�@�&�@���@���@��u@� �@��w@��@�C�@��@���@�^5@�{@��@���@�p�@�&�@���@��9@�Z@� �@�b@��
@��@���@�|�@�l�@�C�@�ȴ@�ff@�5?@��#@��h@�O�@��@�j@�Q�@�(�@� �@�b@�t�@���@���@�^5@�5?@�J@���@��7@�%@���@��j@��9@���@��@�9X@�(�@�1@��F@�t�@�33@��y@��R@���@�=q@���@��7@�&�@���@�z�@�9X@�1@���@���@�|�@�\)@�K�@�+@��@��+G�O�@�Q�@��@�j@|�@o+@d�@W�@Ol�@I��@D9X@>ff@7+@0A�@)7L@#o@�-@��@��@@\)@C�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	ƨB	ƨB	ŢB	ŢB	ŢB	ŢB	ŢB	ŢB	ƨB	ƨB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�BB	�ZB	�B
#�B
I�B
x�B
��B
��B
��B
��B
��B
��B
��B
�B
�9B
�qB
ĜB
ɺB
�B
�B �B+B��B�}B��B�BVB(�BM�BP�B]/BgmBl�B~�B�=B�wB�mB�B�B�yB�BB�B�mBJB�BhB��B�ZB�)B�/B�B��B�dB��B��B�hB�+B�B�Bv�BhsBXBA�BoB�B�BB��B�^B�'B��B�\Bv�BcTBT�B]/B`BBP�B9XB(�B�BJB
��B
�B
�5B
��B
�^B
��B
�dB
�B
ZB
1'B
�B	��B	�fB	��B	��B	z�B	]/B	J�B	H�B	H�B	E�B	<jB	/B	�B	uB	bB	JB	B��B	B	B��B��B�B�yB�HB�TB�fB�B�B�B�B�B�B	B	\B	PB	JB	
=B	
=B		7B	1B		7B		7B	1B	1B	+B	B�B�#B��B��B�jB��B��B�uB�=B�B�B�B�Bz�Bp�Bp�Bw�B~�B}�B�hB�9B��BŢB��B��B	\B	�B	"�B	#�B	&�B	/B	/B	/B	0!B	.B	,B	)�B	)�B	)�B	/B	33B	6FB	;dB	<jB	?}B	D�B	I�B	R�B	ZB	cTB	k�B	v�B	�+B	�JB	�JB	�=B	�1B	�DB	�1B	�B	�B	�B	�%B	�B	w�B	z�B	|�B	�B	�DB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�RB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�#B	�/B	�;B	�BB	�BB	�HB	�fB	�fB	�mB	�mB	�fB	�mB	�mB	�mB	�mB	�sB	�yB	�yB	�mB	�fB	�`B	�ZB	�BB	�/B	�)B	�5B	�B	��B	��B	��B	��B	�}B	�qB	�dB	�dB	�XB	�RB	�dB	�?B	�B	��B	��B	��B	��B	�B	��B	��B	�B	�B	��B	�?B	ŢB	ɺB	ƨB	ƨB	��B	��B	��B	��B	��B	��B	��B	ŢB	�qB	ÖB	ǮB	ǮB	ǮB	ǮB	ƨB	ŢB	ŢB	ÖB	��B	B	ĜB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�5B	�BB	�NB	�TB	�fB	�mB	�sB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
+B
1B

=B

=B

=B
DB
DB
DB
PB
PB
PB
PB
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
hB
hB
oB
uB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
"�B
+B
33B
<jB
C�B
G�B
K�B
O�B
XB
]/B
e`B
jB
o�B
r�B
v�B
z�B
}�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B	ƟB	ơB	ŚB	ŚB	ŜB	ŜB	śB	śB	ƠB	ƠB	ƟB	ơB	ʻB	˾B	��B	��B	ʺB	ʺB	ʹB	˾B	˽B	�B	�:B	�RB	�B
#�B
I�B
x�B
��B
��B
��B
��B
��B
��B
��B
�B
�+B
�aB
ďB
ɩB
�B
�oB �B*�B�oB�kB˳B�BAB(�BM�BP�B]BgVBlyB~�B�*B�fB�YB�B�qB�eB�0B�B�[B4BvBRB��B�IB�B�B�B��B�PB��B��B�UB�B��B��Bv�Bh`BW�BAsBYB�B�.B̶B�IB�B��B�FBv�Bc?BT�B]B`-BP�B9@B(�B�B7B
��B
�B
�B
̷B
�KB
��B
�MB
��B
Z
B
1B
nB	��B	�XB	˷B	�}B	z�B	]"B	J�B	H�B	H�B	E�B	<aB	/B	�B	mB	YB	AB	 �B��B	 B	 �B��B��B�B�qB�@B�LB�\B�xB�B�B�B�B�B	B	OB	DB	<B	
1B	
.B		-B	&B		+B		,B	&B	%B	B	B�B�B��B��B�aB��B�xB�mB�6B�B�B�B�Bz�Bp�Bp�Bw�B~�B}�B�`B�/B�~BŘB��B��B	OB	B	"�B	#�B	&�B	/B	/
B	/B	0B	.B	+�B	)�B	)�B	)�B	/B	3%B	66B	;SB	<[B	?mB	D�B	I�B	R�B	ZB	cBB	kuB	v�B	�B	�8B	�4B	�,B	�B	�2B	�!B	�B	� B	�B	�B	��B	w�B	z�B	|�B	�B	�2B	�DB	�kB	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�?B	ŋB	ǘB	ʮB	˳B	̶B	˱B	ʬB	ʭB	ɥB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�%B	�*B	�,B	�2B	�OB	�PB	�XB	�XB	�OB	�WB	�ZB	�VB	�UB	�_B	�cB	�dB	�UB	�PB	�IB	�EB	�,B	�B	�B	�B	�B	ʫB	�tB	�mB	�oB	�hB	�ZB	�OB	�NB	�AB	�>B	�NB	�)B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�*B	ŋB	ɣB	ƑB	ƏB	ʬB	ʭB	;B	��B	��B	��B	̶B	ŌB	�\B	�~B	ǙB	ǛB	ǗB	ǘB	ƓB	ŌB	ŋB	ÀB	�rB	�wB	ćB	ƒB	ƒB	ȜB	ʪB	̴B	̲B	̴B	̸B	;B	ͼB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�5B	�=B	�LB	�VB	�]B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B
 �B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
B
B
B
B
B
B
B
B

#B

$B

$B
,B
*B
-B
8B
7B
7B
8B
<B
=B
<B
;B
;B
DB
CB
JB
LB
JB
GB
MB
LB
VB
]B
\B
ZB
^B
[B
aB
fG�O�B
�B
uB
�B
"�B
*�B
3B
<OB
C|B
G�B
K�B
O�B
W�B
]B
eFB
jbB
o�B
r�B
v�B
z�B
}�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.24 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451242016080714512420160807145124  AO  ARCAADJP                                                                    20151206101658    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151206101658  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151206101658  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145124  IP                  G�O�G�O�G�O�                