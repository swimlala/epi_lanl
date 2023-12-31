CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-08T02:15:58Z AOML 3.0 creation; 2016-08-07T21:36:39Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20151008021558  20160807143639  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               LA   AO  5286_8897_076                   2C  D   APEX                            6531                            072314                          846 @�u:+Y)1   @�u:�@��@2�&�x���cA�Q�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    LA   B   B   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`��Bf��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt` Dy` D�fD�L�D�y�D��fD�3D�9�D�vfD���D�  D�S3D�Y�DǼ�D��D�I�Dڌ�D๚D�fD�P D�3D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z@�z�A�
A#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��BaBgBp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�BԮB�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qCWC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dto\Dyo\D�D�T{D��HD��D��D�AHD�~D��{D��D�Z�D�aHD��{D�{D�QHDڔ{D��HD�D�W�D��D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�-A�+A�33A�&�A��A��A�bA�bA�VA�oA�VA�bA�bA�1A�%A�  A�  A�  A�  A�  A�  A�A�  A���A���A��A���A�JA�%A�"�A�ĜA��`A��A̮A�=qA�E�A�bA�I�Aȏ\AǃA���A��#AŸRA�%A��A�XA�ȴA�S�A��7A��9A��A�XA�oA���A��A��A��9A�K�A��A�$�A�p�A��RA�&�A���A��A���A��A�;dA�ĜA�I�A��jA���A���A�S�A�&�A�&�A�ĜA�
=A��7A���A��-A��HA��A���A���A�G�A���A�bA��TA��A�A�bNA��7A�^5A�JA���A�G�A��yA��A�bA���A�+A�?}A�&�A�S�A��^A�-A��^A�ȴA�hsA�XA�wAyC�AvZAuO�Ar�uAq��Ap�9AnĜAmt�Ai/Ae�7Aa�A_l�AZ9XAXbNAW7LAT�9AP��AOAOK�AN1'AJ��AGG�AF�9AE\)AC�
AB��A@9XA?K�A>�A<v�A;�A9ƨA8��A7�A6�\A5��A5�hA4�RA3p�A01'A.bNA.A,��A+7LA* �A)\)A)/A(�A'�hA'��A'�PA&=qA$ZA#ƨA#�PA#hsA"�!A!�
A �Al�A;dA�AA�A"�Ax�A��A=qA�A�-AVA�+AM�A  A��A��A
=A �A33AI�Al�A��A�A~�A
�A	�hA �AJA��A7LA�9A/A�wA��A;dA ~�@��@�ff@�J@���@�?}@�  @�^5@��@�M�@�$�@��T@�%@�bN@��;@��@�K�@���@��@���@�1'@�dZ@��y@ꟾ@�E�@��#@�V@�@��@�l�@���@�$�@�p�@�V@���@�"�@�^@���@߮@�~�@۝�@ڇ+@�X@�z�@�A�@�9X@�j@��@أ�@�M�@�/@ͺ^@��m@˝�@�  @�-@Ь@�\)@�x�@ˮ@���@�(�@���@ɩ�@ʟ�@�C�@��;@̬@̼j@�-@�j@��@�v�@���@Ɨ�@�n�@��^@��9@�  @�  @� �@�&�@�z�@��@�~�@���@��@���@�
=@�V@�$�@��@�J@�ff@���@�V@��@��@�`B@��@��@�t�@��H@�-@��^@�O�@�/@��@��@�V@���@��@���@�ƨ@���@��F@�  @���@�$�@��@�S�@�\)@�ȴ@�;d@��H@�v�@�$�@���@��T@��-@��7@���@�Z@��
@��@���@��m@�9X@�  @���@�^5@�V@�M�@���@���@�z�@�Z@��;@�"�@�=q@��-@��7@��@�x�@�O�@��@�&�@��@�%@��/@���@�I�@� �@�l�@���@�n�@���@�`B@�&�@�V@��@���@�hs@�x�@�=q@��j@��j@�Z@�(�@��@�ƨ@���@�bN@��u@�r�@��w@��@��w@���@�\)@�V@���@�/@��
@�l�@�\)@�S�@�\)@�33@�?}@�\)@�
=@�C�@��H@�
=@�
=@��!@�=q@���@�ƨ@� �@�bN@�(�@�b@�Q�@�p�@��-@�M�@��H@���@��y@���@��+@�@��@��#@��@��@�J@�7L@��9@� �@��@���@�~�@���@��@��@��+@��@��w@��m@��@��
@�K�@���@�p�@�Ĝ@��@�|�@�S�@�33@�@��@�"�@��@���@��H@��H@��y@��y@���@��y@�~�@�J@��^@�Ĝ@���@���@���@���@��P@�n�@�M�@��#@�V@���@�z�@��@��
@���@��@��@~�y@v��@h�u@`�@X�9@R��@Hr�@>�R@7l�@1�^@+��@(Q�@#�@�y@�^@�h@hs@9X@�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�-A�+A�33A�&�A��A��A�bA�bA�VA�oA�VA�bA�bA�1A�%A�  A�  A�  A�  A�  A�  A�A�  A���A���A��A���A�JA�%A�"�A�ĜA��`A��A̮A�=qA�E�A�bA�I�Aȏ\AǃA���A��#AŸRA�%A��A�XA�ȴA�S�A��7A��9A��A�XA�oA���A��A��A��9A�K�A��A�$�A�p�A��RA�&�A���A��A���A��A�;dA�ĜA�I�A��jA���A���A�S�A�&�A�&�A�ĜA�
=A��7A���A��-A��HA��A���A���A�G�A���A�bA��TA��A�A�bNA��7A�^5A�JA���A�G�A��yA��A�bA���A�+A�?}A�&�A�S�A��^A�-A��^A�ȴA�hsA�XA�wAyC�AvZAuO�Ar�uAq��Ap�9AnĜAmt�Ai/Ae�7Aa�A_l�AZ9XAXbNAW7LAT�9AP��AOAOK�AN1'AJ��AGG�AF�9AE\)AC�
AB��A@9XA?K�A>�A<v�A;�A9ƨA8��A7�A6�\A5��A5�hA4�RA3p�A01'A.bNA.A,��A+7LA* �A)\)A)/A(�A'�hA'��A'�PA&=qA$ZA#ƨA#�PA#hsA"�!A!�
A �Al�A;dA�AA�A"�Ax�A��A=qA�A�-AVA�+AM�A  A��A��A
=A �A33AI�Al�A��A�A~�A
�A	�hA �AJA��A7LA�9A/A�wA��A;dA ~�@��@�ff@�J@���@�?}@�  @�^5@��@�M�@�$�@��T@�%@�bN@��;@��@�K�@���@��@���@�1'@�dZ@��y@ꟾ@�E�@��#@�V@�@��@�l�@���@�$�@�p�@�V@���@�"�@�^@���@߮@�~�@۝�@ڇ+@�X@�z�@�A�@�9X@�j@��@أ�@�M�@�/@ͺ^@��m@˝�@�  @�-@Ь@�\)@�x�@ˮ@���@�(�@���@ɩ�@ʟ�@�C�@��;@̬@̼j@�-@�j@��@�v�@���@Ɨ�@�n�@��^@��9@�  @�  @� �@�&�@�z�@��@�~�@���@��@���@�
=@�V@�$�@��@�J@�ff@���@�V@��@��@�`B@��@��@�t�@��H@�-@��^@�O�@�/@��@��@�V@���@��@���@�ƨ@���@��F@�  @���@�$�@��@�S�@�\)@�ȴ@�;d@��H@�v�@�$�@���@��T@��-@��7@���@�Z@��
@��@���@��m@�9X@�  @���@�^5@�V@�M�@���@���@�z�@�Z@��;@�"�@�=q@��-@��7@��@�x�@�O�@��@�&�@��@�%@��/@���@�I�@� �@�l�@���@�n�@���@�`B@�&�@�V@��@���@�hs@�x�@�=q@��j@��j@�Z@�(�@��@�ƨ@���@�bN@��u@�r�@��w@��@��w@���@�\)@�V@���@�/@��
@�l�@�\)@�S�@�\)@�33@�?}@�\)@�
=@�C�@��H@�
=@�
=@��!@�=q@���@�ƨ@� �@�bN@�(�@�b@�Q�@�p�@��-@�M�@��H@���@��y@���@��+@�@��@��#@��@��@�J@�7L@��9@� �@��@���@�~�@���@��@��@��+@��@��w@��m@��@��
@�K�@���@�p�@�Ĝ@��@�|�@�S�@�33@�@��@�"�@��@���@��H@��H@��y@��y@���@��y@�~�@�J@��^@�Ĝ@���@���@���@���@��P@�n�@�M�@��#@�V@���@�z�@��@��
G�O�@��@��@~�y@v��@h�u@`�@X�9@R��@Hr�@>�R@7l�@1�^@+��@(Q�@#�@�y@�^@�h@hs@9X@�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB>wB>wB?}B>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB>wB?}B?}B?}B?}B?}BC�BJ�BW
B�=B��B��BŢB�
B�/B�mB%B'�B:^BH�BgmBu�Bu�B�+B�\B��B�B�'BÖB�B�fB�B��B��B��B��BB+B1B\BDB
=BB%B
=B%B��B��B�B�TB�BB�/B��BǮB�qB�jB�}B�XB�FB�B��B��B�PB�7B}�Bk�BcTBZBJ�B:^B1'B/B%�B�B  B�TB�B��B�9B��B}�BG�B	7B
�B
ȴB
��B
��B
��B
��B
t�B
G�B
.B
�B	�ZB	�B	��B	��B	�LB	�B	��B	��B	w�B	^5B	G�B	5?B	�B	bB	1B��B�B�mB�ZB�/B��BŢBB�dB�FB�-B�B��B��B��B��B��B��B�B�-B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�LB�XB�XB�LB�LB�qB�}B�wB�jB�XB�9BÖBŢBÖB�}B�dB�3B��B��B��B��B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�'B�'B�B�B�B�B�B��B��B��B��B��B��B�B�'B�9B�?B�FB�XB�jBŢBƨBŢBǮBȴB��B��B��B��B��B��B��B��B��BƨBĜB��B�}B��BÖBŢB��B��BŢB�3B�B�!B�3B�LBƨB�BB�B��B��BŢB��BǮB�`B�B��B��B	B	B��B��B	hB	uB	PB	B��B�`B��B��B��B�B�HB�;B�B�
B�
B�B�B�;B�;B�`B�sB�B�B�B��B��B��B��B	  B	B	%B		7B	VB	{B	�B	�B	�B	�B	�B	�B	 �B	(�B	(�B	)�B	+B	.B	7LB	6FB	:^B	D�B	H�B	J�B	O�B	N�B	M�B	P�B	P�B	Q�B	Q�B	R�B	VB	S�B	R�B	S�B	ZB	\)B	_;B	aHB	_;B	_;B	_;B	aHB	hsB	q�B	x�B	z�B	|�B	� B	�B	�B	�B	�B	�B	�B	�+B	�1B	�7B	�=B	�7B	�DB	�hB	�hB	�\B	�PB	�\B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	�RB	�dB	�dB	�jB	�jB	�qB	�}B	ÖB	ŢB	ĜB	ĜB	ƨB	ɺB	ɺB	��B	ȴB	ƨB	ŢB	B	B	B	B	B	��B	�jB	�^B	�^B	�wB	�wB	�}B	�}B	�}B	�wB	B	��B	��B	��B	��B	��B	��B	�)B	�5B	�HB	�`B	�`B	�`B	�`B	�`B	�TB	�ZB	�fB	�ZB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
1B
�B
�B
%�B
2-B
7LB
<jB
D�B
J�B
P�B
W
B
\)B
`BB
dZB
iyB
l�B
q�B
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B>gB>fB?jB>hB>gB>gB>fB>gB>gB>gB>eB>hB>fB>eB>hB>eB>fB>eB>gB>eB?mB?lB?kB?kB?iBC�BJ�BV�B�-B��B�qBŏB��B�B�XBB'�B:LBH�BgZBu�Bu�B�B�FB��B��B�BÁB�
B�TB�B��B��B��B��BBBBNB1B
*BBB
+BB��B��B�rB�@B�.B�B��BǙB�[B�VB�jB�AB�1B�B��B��B�:B�B}�BkpBc=BZBJ�B:EB1B/B%�BxB��B�=B�B��B�!B��B}�BG�B	 B
�}B
ȟB
�mB
ʬB
�tB
��B
t�B
G�B
.B
�B	�KB	��B	��B	�uB	�>B	�B	��B	�yB	w�B	^)B	G�B	55B	�B	YB	)B��B�}B�dB�TB�&B��BśBB�\B�>B�(B�B��B��B��B��B��B��B��B�%B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�DB�OB�NB�CB�BB�gB�sB�mB�`B�MB�1BÍBŗBÍB�sB�ZB�*B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�
B�B�B��B��B��B��B��B��B��B��B�B�1B�4B�=B�MB�^BŖBƛBŕBǟBȧBʸB˺B��B��B��B��B��BʶB��BƚBĒB�vB�qB�zBÌBŖB��B��BŕB�'B��B�B�(B�BBƚB�4B�B��B˻BŕB�}BǟB�RB�B��B��B	 �B	B��B��B	XB	dB	@B	B��B�QB��B��B��B�B�8B�-B�B��B��B��B�	B�.B�,B�PB�dB�vB�B�B��B��B��B��B��B	 �B	B		%B	FB	hB	�B	�B	�B	�B	�B	�B	 �B	(�B	(�B	)�B	*�B	.B	78B	63B	:LB	D�B	H�B	J�B	O�B	N�B	M�B	P�B	P�B	Q�B	Q�B	R�B	U�B	S�B	R�B	S�B	ZB	\B	_)B	a5B	_(B	_)B	_&B	a4B	h`B	q�B	x�B	z�B	|�B	�B	��B	��B	��B	��B	��B	�
B	�B	�B	�!B	�)B	�"B	�1B	�TB	�TB	�BB	�;B	�FB	�XB	�`B	�gB	�vB	��B	��B	��B	��B	��B	�;B	�PB	�NB	�TB	�TB	�\B	�dB	�}B	ŋB	ąB	ĆB	ƎB	ɤB	ɣB	ʪB	ȝB	ƓB	ŉB	�xB	�xB	�xB	�zB	�{B	�tB	�VB	�HB	�HB	�cB	�aB	�gB	�eB	�fB	�bB	�vB	˱B	ͼB	��B	��B	��B	��B	�B	�B	�0B	�IB	�JB	�JB	�JB	�IB	�>B	�AB	�OB	�CB	�uB	�B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�kB	�gB	�ZB	�ZB	�aB	�aB	�nB	�xB	�{B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�xB	�rB	�rB	�eB	�fB	�eG�O�B	�B	��B
B
�B
�B
%�B
2B
71B
<PB
D�B
J�B
P�B
V�B
\B
`'B
d>B
i`B
lpB
q�B
u�B
x�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.24 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436392016080714363920160807143639  AO  ARCAADJP                                                                    20151008021558    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151008021558  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151008021558  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143639  IP                  G�O�G�O�G�O�                