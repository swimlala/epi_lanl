CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-02-22T03:15:59Z AOML 3.0 creation; 2016-08-07T21:36:44Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160222031559  20160807143644  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               fA   AO  5286_8897_102                   2C  D   APEX                            6531                            072314                          846 @ח��GM�1   @ח�G4@2��n���cH��
=q1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    fA   B   B   @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D%��D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDyy�D�	�D�L�D�|�D��fD���D�C3D���D�� D���D�6fD�i�D�ɚD�	�D�0 Dڠ D�fD�3D�6fD�s3D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @H��@�z�@ǮA�
A#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B��B��B��B ��B(��B1\)B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��B��B��B�G�B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D	\D	�\D
\D
�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&�D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS�\DT\DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dtu�Dy��D�HD�T{D��{D��D�{D�J�D��HD�׮D��{D�>D�qHD��HD�HD�7�Dڧ�D�D�
�D�>D�z�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A�r�A�O�A�C�A�;dA�1'A�(�A��A��A�oA�bA�VA�JA�
=A�1A�%A�%A�%A�1A�A�  A���A���A���A���A���A���A��A��A��A��A�1A�/A�=qA�Q�A�p�AǬA��A�+AǶFA�A�"�Ař�A�x�A��A�/A��TA��A���A� �A�1'A�?}A���A��
A�C�A�33A�hsA�A��7A��
A��A�XA�z�A�dZA�33A�bNA���A���A���A�M�A�~�A�I�A�+A��;A�Q�A���A�z�A�A�A���A��FA��A���A�$�A�l�A�5?A�VA���A�ĜA�S�A���A��uAƨA}��A|�uA{��Ay�Av5?Au33Ar�Al$�Ad�/AadZA_�wAW�;AO��AM�-AIK�ADZA=��A<�A;XA:��A9�A7�A6��A6�+A5��A3S�A/��A-��A,��A,�9A,�A,$�A+S�A*^5A)�TA)��A)p�A)�A(�A'XA%�A#VA"E�A!��A!�PA n�A�PA+A��A��A{A?}A��A��A�wA�!A-A��A{A;dA�A��AK�A��AA�A�A��A
ȴA
��A
v�A
 �A�A�TAG�AVA�A�DA$�A��A?}A��A�wA��Ax�A?}A�A n�@��+@��`@���@��@��u@��R@��#@�x�@���@��;@�ȴ@�`B@���@�\)@�\@�Ĝ@��;@�S�@�@��;@柾@���@�p�@���@�z�@��@��@ݩ�@��#@؃@���@ՙ�@�Q�@�33@�ff@�@�`B@���@��m@��H@�p�@���@˝�@�ff@�hs@�Z@�
=@Ɨ�@�v�@�@Ĵ9@��;@�o@�-@���@���@�%@�\)@���@�E�@�{@��^@�`B@��@�1@�S�@��!@�V@�$�@��^@��@�b@���@�o@���@�M�@���@�Ĝ@�Q�@�l�@�~�@��#@���@��7@�X@�7L@��@��j@�b@���@�-@�J@��-@�7L@� �@�(�@�(�@�  @��
@���@�t�@�;d@��y@�v�@��@�X@��@���@���@��@�1@�9X@��@��@�z�@�bN@�Q�@��;@���@�5?@�@��T@���@���@��@���@���@��/@�1'@��@��w@��@�l�@�C�@��H@�M�@�@�@��h@�?}@��@�I�@�I�@���@��w@��@���@�t�@�@�=q@���@���@�hs@��9@� �@��@��P@�|�@�\)@�33@��@��y@���@��R@��!@�~�@�M�@�@�V@���@�j@�A�@�b@���@��@�|�@�l�@�S�@�33@�@��\@�?}@���@�r�@�I�@�ƨ@�+@�~�@��@���@�&�@�%@��@��/@���@��j@�r�@��@���@�|�@�\)@�S�@�C�@�;d@�+@�@��@��@�ȴ@���@��\@�-@���@�@��^@���@��h@��@�`B@�G�@��@���@��@���@��9@��u@�I�@�  @��m@���@���@�;d@�+@�"�@�o@��@��R@��\@�M�@���@���@��-@�x�@�?}@��@��j@�Ĝ@�Ĝ@��@���@��u@�I�@���@���@��@�\)@�S�@�33@�o@�n�@��#@�`B@��@��/@��u@�z�@�bN@�Q�@� �@���@�33@�o@�o@��!@��\@��+@�~�@�V@��@�{@�@�@�@���@���@��@�hs@�G�@�&�@�V@���@��/@��u@�Q�@�b@�  @�;@|�@~�+@~V@~$�@}�@}��@}?}@|�/@|9X@{S�@{@z�H@w��@m/@ct�@\1@T�@J�\@C�
@<�j@6�@/�@(A�@#S�@V@�@��@X@@S�@��@��@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A�r�A�O�A�C�A�;dA�1'A�(�A��A��A�oA�bA�VA�JA�
=A�1A�%A�%A�%A�1A�A�  A���A���A���A���A���A���A��A��A��A��A�1A�/A�=qA�Q�A�p�AǬA��A�+AǶFA�A�"�Ař�A�x�A��A�/A��TA��A���A� �A�1'A�?}A���A��
A�C�A�33A�hsA�A��7A��
A��A�XA�z�A�dZA�33A�bNA���A���A���A�M�A�~�A�I�A�+A��;A�Q�A���A�z�A�A�A���A��FA��A���A�$�A�l�A�5?A�VA���A�ĜA�S�A���A��uAƨA}��A|�uA{��Ay�Av5?Au33Ar�Al$�Ad�/AadZA_�wAW�;AO��AM�-AIK�ADZA=��A<�A;XA:��A9�A7�A6��A6�+A5��A3S�A/��A-��A,��A,�9A,�A,$�A+S�A*^5A)�TA)��A)p�A)�A(�A'XA%�A#VA"E�A!��A!�PA n�A�PA+A��A��A{A?}A��A��A�wA�!A-A��A{A;dA�A��AK�A��AA�A�A��A
ȴA
��A
v�A
 �A�A�TAG�AVA�A�DA$�A��A?}A��A�wA��Ax�A?}A�A n�@��+@��`@���@��@��u@��R@��#@�x�@���@��;@�ȴ@�`B@���@�\)@�\@�Ĝ@��;@�S�@�@��;@柾@���@�p�@���@�z�@��@��@ݩ�@��#@؃@���@ՙ�@�Q�@�33@�ff@�@�`B@���@��m@��H@�p�@���@˝�@�ff@�hs@�Z@�
=@Ɨ�@�v�@�@Ĵ9@��;@�o@�-@���@���@�%@�\)@���@�E�@�{@��^@�`B@��@�1@�S�@��!@�V@�$�@��^@��@�b@���@�o@���@�M�@���@�Ĝ@�Q�@�l�@�~�@��#@���@��7@�X@�7L@��@��j@�b@���@�-@�J@��-@�7L@� �@�(�@�(�@�  @��
@���@�t�@�;d@��y@�v�@��@�X@��@���@���@��@�1@�9X@��@��@�z�@�bN@�Q�@��;@���@�5?@�@��T@���@���@��@���@���@��/@�1'@��@��w@��@�l�@�C�@��H@�M�@�@�@��h@�?}@��@�I�@�I�@���@��w@��@���@�t�@�@�=q@���@���@�hs@��9@� �@��@��P@�|�@�\)@�33@��@��y@���@��R@��!@�~�@�M�@�@�V@���@�j@�A�@�b@���@��@�|�@�l�@�S�@�33@�@��\@�?}@���@�r�@�I�@�ƨ@�+@�~�@��@���@�&�@�%@��@��/@���@��j@�r�@��@���@�|�@�\)@�S�@�C�@�;d@�+@�@��@��@�ȴ@���@��\@�-@���@�@��^@���@��h@��@�`B@�G�@��@���@��@���@��9@��u@�I�@�  @��m@���@���@�;d@�+@�"�@�o@��@��R@��\@�M�@���@���@��-@�x�@�?}@��@��j@�Ĝ@�Ĝ@��@���@��u@�I�@���@���@��@�\)@�S�@�33@�o@�n�@��#@�`B@��@��/@��u@�z�@�bN@�Q�@� �@���@�33@�o@�o@��!@��\@��+@�~�@�V@��@�{@�@�@�@���@���@��@�hs@�G�@�&�@�V@���@��/@��u@�Q�@�b@�  @�;@|�@~�+@~V@~$�@}�@}��@}?}@|�/@|9X@{S�@{G�O�@w��@m/@ct�@\1@T�@J�\@C�
@<�j@6�@/�@(A�@#S�@V@�@��@X@@S�@��@��@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
&�B
&�B
+B
.B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
/B
/B
/B
/B
.B
/B
0!B
9XB
VB
n�B
}�B
�B
�TBBXBx�B��B��B�B��B��B��B��B��B�B
=B1B�B�`BBPBB�B9XB-B �B�B!�BhB��B�B�BB��B�}B�dB�FB��Bs�BI�B6FB'�B�B�B
��B
�mB
��B
��B
�B
r�B
]/B
XB
YB
L�B
(�B
�B
bB
%B	��B	�9B	��B	��B	�hB	�B	l�B	cTB	L�B	.B	\B��B�B��B�?B��B��B�{B�VB�+B�B�Bz�Bx�Bw�Bt�Bq�Bk�BgmBgmBgmBgmBgmBgmBiyBjBk�Bk�Bk�BjBhsBgmBffBgmBhsBiyBhsBo�Bt�Bx�Bz�B|�B�B�B�%B�%B�%B�B�B�B�Bz�Br�Bq�Bp�Bp�Bo�Bp�Bv�Bw�Bv�Bu�Bt�Bs�Bq�Bp�Bo�Bm�Bm�Bm�Bm�Bm�Bl�Bo�Bo�Bs�Bv�Bv�Bv�Bx�Bx�Bw�Bu�Bu�Bu�Bt�Bt�Bt�Bs�Bs�Bs�Bt�Br�Bq�Br�Bq�Bo�Bn�Bm�Bm�Bm�Bm�Bn�Bo�Bo�Bm�Bk�Bm�Bk�Bk�Bl�Bn�Bq�Bs�Br�Bs�Bw�Bx�Bz�B|�B|�B� B�B�B�+B�DB�JB�JB�PB�hB�{B��B��B��B��B��B��B��B��B�B�B�B�!B�9B�LB�^B�jB�qB�}BǮBɺB��B��B��B��B�B�/B�;B�NB�mB�B�B�B�B�B�B��B��B��B	B	B	B	+B	\B	hB	{B	�B	�B	�B	�B	�B	�B	 �B	"�B	#�B	!�B	"�B	%�B	%�B	'�B	)�B	0!B	33B	5?B	7LB	7LB	9XB	>wB	A�B	B�B	C�B	F�B	H�B	L�B	O�B	R�B	W
B	]/B	_;B	`BB	`BB	aHB	aHB	cTB	e`B	ffB	hsB	jB	iyB	jB	k�B	k�B	m�B	n�B	o�B	p�B	p�B	s�B	y�B	}�B	}�B	�B	�B	�1B	�DB	�JB	�JB	�PB	�VB	�bB	�oB	�{B	��B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�9B	�FB	�RB	�XB	�dB	�wB	��B	��B	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ƨB	ǮB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�)B	�)B	�/B	�/B	�;B	�BB	�HB	�HB	�NB	�TB	�`B	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
JB
VB
�B
#�B
+B
2-B
:^B
?}B
F�B
K�B
R�B
YB
]/B
cTB
hsB
l�B
p�B
r�B
v�B
x�B
{�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
&�B
&�B
*�B
.B
/B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
0B
/B
/B
/B
/B
.	B
/B
0B
9JB
U�B
n�B
}�B
��B
�BBBW�Bx�B�nB��B��B��B��B��B��B��B�}B
*BB�B�KB�B8BB{B9CB,�B �B�B!�BTB��B�B�,B̶B�fB�MB�/B��Bs�BI�B6/B'�B�BkB
��B
�VB
��B
��B
��B
r�B
]B
W�B
YB
L�B
(�B
�B
UB
B	��B	�)B	��B	��B	�[B	� B	l}B	cHB	L�B	.B	TB��B�B��B�:B��B��B�uB�PB�'B�B�Bz�Bx�Bw�Bt�Bq�Bk�BgjBgkBgjBgjBghBghBitBj{Bk�Bk�BkBjxBhpBgjBfcBgiBhrBiuBhpBo�Bt�Bx�Bz�B|�B�B�B�B�B�B�B�B�B�Bz�Br�Bq�Bp�Bp�Bo�Bp�Bv�Bw�Bv�Bu�Bt�Bs�Bq�Bp�Bo�Bm�Bm�Bm�Bm�Bm�Bl�Bo�Bo�Bs�Bv�Bv�Bv�Bx�Bx�Bw�Bu�Bu�Bu�Bt�Bt�Bt�Bs�Bs�Bs�Bt�Br�Bq�Br�Bq�Bo�Bn�Bm�Bm�Bm�Bm�Bn�Bo�Bo�Bm�Bk|Bm�Bk{Bk}BlBn�Bq�Bs�Br�Bs�Bw�Bx�Bz�B|�B|�B�B�B�B�#B�9B�?B�>B�FB�aB�pB�{B��B��B��B��B��B��B��B��B�B�B�B�+B�@B�QB�[B�dB�qBǝBɮB˷B��B��B��B� B�!B�-B�AB�\B�qB�}B�B�B�B�B��B��B��B	 �B	�B	 B	B	KB	WB	iB	nB	vB	wB	}B	�B	�B	 �B	"�B	#�B	!�B	"�B	%�B	%�B	'�B	)�B	0B	3!B	5-B	78B	7:B	9FB	>fB	AxB	B{B	C�B	F�B	H�B	L�B	O�B	R�B	V�B	]B	_&B	`.B	`-B	a6B	a4B	c@B	eKB	fQB	h_B	jkB	ibB	jiB	kqB	kqB	m�B	n�B	o�B	p�B	p�B	s�B	y�B	}�B	}�B	��B	�B	�B	�.B	�4B	�3B	�<B	�@B	�MB	�XB	�hB	�kB	�eB	�qB	�yB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�.B	�9B	�@B	�OB	�`B	�kB	�rB	�|B	�}B	�}B	ÀB	�~B	ÁB	�B	ăB	ƐB	ǗB	ȝB	țB	ɢB	ɢB	ʪB	˰B	̶B	̶B	ͻB	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�+B	�/B	�/B	�4B	�<B	�GB	�\B	�ZB	�[B	�aB	�mB	�sB	�rB	�sB	�xB	�B	�~B	�B	�B	�B	��B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B	��B
�B
�B
�B
�B
�B
�B
�B
�B
 B
B
B
B
B

B
B
B
B
B
B
B
B
B
B
B
B
B
	B
	B
	B
	B
	B
	B

$B
+B
+G�O�B
>B
�B
#�B
*�B
2B
:FB
?`B
F�B
K�B
R�B
X�B
]B
c8B
hWB
loB
p�B
r�B
v�B
x�B
{�B
~�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.24 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436442016080714364420160807143644  AO  ARCAADJP                                                                    20160222031559    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160222031559  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160222031559  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143644  IP                  G�O�G�O�G�O�                