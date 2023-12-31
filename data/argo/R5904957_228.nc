CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:47Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024140847  20181024140847  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��sK�
1   @���mP@5V��d����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @333@y��@���A   A   A@  A`  A�  A�  A�  A���A���A�  A���A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C �C"�C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C�  C�  C�  C�  C��C�  C��3C��3C�  D   D � D  D� D  D� D��D� D  D� D  D� D  D� D  D� DfD� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  Dy�D  D� D  D� D  Dy�D��D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-y�D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D?��D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DVy�DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\�fD]fD]� D^  D^� D_  D_� D`  D`� D`��Da� Db  Db�fDc  Dc� DdfDd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Ds��Dty�Dt��Du� Dv  Dvy�Dw  Dw� Dw� Dy�)D�$�D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @B�\@�z�@�z�A�
A#�
AC�
Ac�
A��A��A��A��RA¸RA��A�RA��B ��B��B��B��B ��B(��B0��B9\)B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B��B�z�B��B��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC WC"WC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4WC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C�+�C�+�C�+�C�+�C�+�C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C�+�C��C��C��C��D \D �\D\D�\D\D�\D�D�\D\D�\D\D�\D\D�\D\D�\D�D�\D	\D	�\D
\D
�\D\D�\D�D�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D��D\D�\D\D�\D\D��D�D�\D\D��D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-\D-��D.�D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9��D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@�D@�\DA�DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO��DP\DP�\DQ\DQ�\DR\DR��DS\DS�\DT\DT�\DU\DU�\DV\DV��DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\��D]�D]�\D^\D^�\D_\D_�\D`\D`�\Da�Da�\Db\Db��Dc\Dc�\Dd�Dd��De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl\Dl��Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt�Dt��Du�Du�\Dv\Dv��Dw\Dw�\Dw�\Dy��D�,{D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�`BA�`BA�bNA�bNA�`BA�bNA�bNA�dZA�ffA�l�A�jA�jA�hsA�hsA�hsA�ZA�?}A�;dA�1A��A��#A���A���AǼjAǸRA���A��/A��A��HA�hsA�  Aơ�A�ȴA�A�A�oA�ƨA�bNA�A�Q�A��A��yA���A�?}A�bNA���A���A��yA��jA���A�?}A�v�A���A�p�A�
=A�bA���A�|�A�ffA��mA�9XA�M�A�x�A���A���A��A�1'A���A�|�A��RA�;dA��A�\)A�A���A���A�r�A�%A�z�A�33A��7A�1'A��A�bNA���A�
=A��A�33A�"�A���A�bNA�?}A�(�A��A�;dA�C�A�"�A�&�A���A��hA���A�VA�oA�$�A���A�\)A�A��A�A|�!Az�RAydZAw`BAv$�At��As�PAq��AoC�AmXAm7LAmC�Al�yAf�9Ac��AcXA`~�A\9XA[XAZAY�AYoAW�7AV1'AU\)AS33AQC�APffAM��AL�9AJȴAJ �AI|�AH�AG�
AFĜAE��AC&�A@��A?��A?G�A>$�A;��A:�yA:ZA9�mA8�A7�-A6=qA3�A3�PA3`BA3�A1��A.�A,bNA*��A)C�A(��A'�#A'�^A'�PA&�A%l�A%�A$ �A#O�A"z�A!%A�wA&�A�;A��A�A�DAAE�A?}A~�A�A�wA�AoA�AffA��A��AVA�/A�A	��AĜA�wA+A�FA��An�A(�A�A��AVA�A�mA�FA�FA?}A ~�A A�A {@��;@�l�@�V@���@���@��9@�b@�S�@��y@�=q@��@��@�F@�@��@�p�@�j@��@�I�@�ȴ@��@���@�V@�O�@�I�@��@�$�@�b@�C�@݉7@��@��`@ܣ�@ڏ\@���@��m@��@��H@պ^@ԋD@�A�@���@�G�@���@��@Ϯ@�dZ@�"�@�V@��`@�r�@˝�@���@ə�@�Ĝ@��m@�\)@�M�@�?}@ě�@ă@�I�@�9X@��@�=q@�?}@���@���@�A�@�1'@� �@�  @�l�@��+@�@��@��;@�&�@�^5@�r�@���@��;@�C�@��R@�n�@���@�7L@���@�r�@��@��P@��y@�^5@��-@�1@���@�l�@��@�G�@�I�@��@��@�C�@��R@�ff@�M�@�@�`B@�7L@��@�V@�V@��@�7L@�X@�?}@�G�@�X@��j@�(�@�bN@��/@��@�9X@�  @��@��F@��@��R@���@�n�@���@���@��j@��@���@��@�Z@�Z@��;@��+@��@�r�@��@���@��@�G�@��u@��@��H@��@��R@�^5@��@���@���@�Q�@�b@�  @��m@�  @�A�@�Q�@�r�@�z�@���@�Ĝ@�V@�/@�7L@�7L@���@��@��P@��@��R@��\@��+@�=q@��-@�p�@�O�@��@��@���@���@��h@�G�@�hs@��h@�/@�/@�p�@�x�@�G�@�O�@�?}@��@���@��@��j@���@�I�@�  @��@��;@���@�K�@��H@���@�n�@�E�@�@��@���@�p�@�%@���@�  @���@���@��\@�v�@�v�@�~�@�n�@��T@���@��@��@���@�?}@�V@���@��`@��j@�A�@��
@���@���@�5?@��/@�z�@�j@�Ĝ@�?}@�&�@���@��j@�Ĝ@��D@�bN@�1'@��@�S�@��H@���@�n�@�v�@�ff@�5?@��^@��@�p�@�`B@�X@�X@���@��T@���@��@�hs@�O�@�/@��@���@��D@�Z@�s�@wS�@ew21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�`BA�`BA�`BA�bNA�bNA�`BA�bNA�bNA�dZA�ffA�l�A�jA�jA�hsA�hsA�hsA�ZA�?}A�;dA�1A��A��#A���A���AǼjAǸRA���A��/A��A��HA�hsA�  Aơ�A�ȴA�A�A�oA�ƨA�bNA�A�Q�A��A��yA���A�?}A�bNA���A���A��yA��jA���A�?}A�v�A���A�p�A�
=A�bA���A�|�A�ffA��mA�9XA�M�A�x�A���A���A��A�1'A���A�|�A��RA�;dA��A�\)A�A���A���A�r�A�%A�z�A�33A��7A�1'A��A�bNA���A�
=A��A�33A�"�A���A�bNA�?}A�(�A��A�;dA�C�A�"�A�&�A���A��hA���A�VA�oA�$�A���A�\)A�A��A�A|�!Az�RAydZAw`BAv$�At��As�PAq��AoC�AmXAm7LAmC�Al�yAf�9Ac��AcXA`~�A\9XA[XAZAY�AYoAW�7AV1'AU\)AS33AQC�APffAM��AL�9AJȴAJ �AI|�AH�AG�
AFĜAE��AC&�A@��A?��A?G�A>$�A;��A:�yA:ZA9�mA8�A7�-A6=qA3�A3�PA3`BA3�A1��A.�A,bNA*��A)C�A(��A'�#A'�^A'�PA&�A%l�A%�A$ �A#O�A"z�A!%A�wA&�A�;A��A�A�DAAE�A?}A~�A�A�wA�AoA�AffA��A��AVA�/A�A	��AĜA�wA+A�FA��An�A(�A�A��AVA�A�mA�FA�FA?}A ~�A A�A {@��;@�l�@�V@���@���@��9@�b@�S�@��y@�=q@��@��@�F@�@��@�p�@�j@��@�I�@�ȴ@��@���@�V@�O�@�I�@��@�$�@�b@�C�@݉7@��@��`@ܣ�@ڏ\@���@��m@��@��H@պ^@ԋD@�A�@���@�G�@���@��@Ϯ@�dZ@�"�@�V@��`@�r�@˝�@���@ə�@�Ĝ@��m@�\)@�M�@�?}@ě�@ă@�I�@�9X@��@�=q@�?}@���@���@�A�@�1'@� �@�  @�l�@��+@�@��@��;@�&�@�^5@�r�@���@��;@�C�@��R@�n�@���@�7L@���@�r�@��@��P@��y@�^5@��-@�1@���@�l�@��@�G�@�I�@��@��@�C�@��R@�ff@�M�@�@�`B@�7L@��@�V@�V@��@�7L@�X@�?}@�G�@�X@��j@�(�@�bN@��/@��@�9X@�  @��@��F@��@��R@���@�n�@���@���@��j@��@���@��@�Z@�Z@��;@��+@��@�r�@��@���@��@�G�@��u@��@��H@��@��R@�^5@��@���@���@�Q�@�b@�  @��m@�  @�A�@�Q�@�r�@�z�@���@�Ĝ@�V@�/@�7L@�7L@���@��@��P@��@��R@��\@��+@�=q@��-@�p�@�O�@��@��@���@���@��h@�G�@�hs@��h@�/@�/@�p�@�x�@�G�@�O�@�?}@��@���@��@��j@���@�I�@�  @��@��;@���@�K�@��H@���@�n�@�E�@�@��@���@�p�@�%@���@�  @���@���@��\@�v�@�v�@�~�@�n�@��T@���@��@��@���@�?}@�V@���@��`@��j@�A�@��
@���@���@�5?@��/@�z�@�j@�Ĝ@�?}@�&�@���@��j@�Ĝ@��D@�bN@�1'@��@�S�@��H@���@�n�@�v�@�ff@�5?@��^@��@�p�@�`B@�X@�X@���@��T@���@��@�hs@�O�@�/@��@���@��D@�Z@�s�@wS�@ew21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBe`Be`Be`Be`BffBgmBk�Bs�Bt�B�B�DB��B��B��B��B�B��B�B+B�B�BhB
=B��B��B��BB�B%�B#�B�BhB�B�B�B�B=qBT�Be`B�B}�Bo�Bv�B�DB�JB�%Bx�B�Bq�BdZBffBjB_;BP�BH�BC�B@�B/B"�BVB  B�B�HB�B��B��B�3B��B��B��B�VB~�Bx�Bm�BXBP�B6FB"�B �B�B+B
��B
�B
�mB
�#B
ŢB
ÖB
B
��B
�9B
�B
��B
�\B
x�B
^5B
W
B
P�B
H�B
=qB
0!B
�B
!�B
�B
hB
	7B
B	��B	�B	�B	�B	�B	�B	�!B	��B	��B	�B	iyB	bNB	[#B	W
B	VB	O�B	E�B	?}B	5?B	,B	&�B	�B	�B	DB	
=B	+B	B��B��B��B�B�TB�;B�B��B�^B�RB�RB�XB�RB�9B�B�B�'B�-B�B��B��B��B�{B�{B�{B��B��B��B�uB�oB�bB�VB�JB�+B�B�B}�B{�Bw�Bt�Bq�Bo�Bl�Bm�Bm�Bo�Bp�Bq�Bq�Bl�B`BB]/BbNBp�Bk�BffB`BB^5B\)B[#B[#B\)B]/B]/B]/B\)B^5B^5B_;B_;B^5BaHBffBffBffBffBe`BcTBaHB`BB^5B^5B_;B_;BcTBffBgmBhsBhsBjBk�Bk�Bo�Bq�Bp�Bo�Bq�Bp�Bq�Bq�Br�Bu�B{�B~�B�B�+B�7B�DB�uB��B��B��B��B��B��B��B��B�!B�9B�LB�XB�^B�^B�jB�dB�dB�qB�jB�jB�jB�wB�}BBŢBȴBȴBɺB��B��B�B�/B�BB�TB�sB�yB�yB�B�B�B�B�B�fB�TB�B��B	PB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	#�B	$�B	'�B	/B	2-B	33B	8RB	=qB	B�B	D�B	E�B	G�B	J�B	J�B	I�B	L�B	P�B	S�B	W
B	^5B	`BB	aHB	cTB	e`B	gmB	iyB	k�B	o�B	r�B	t�B	y�B	}�B	}�B	|�B	|�B	{�B	|�B	}�B	�B	�B	�%B	�%B	�=B	�\B	�bB	�oB	�oB	�oB	�uB	�hB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�FB	�FB	�LB	�dB	��B	��B	��B	B	B	B	��B	��B	��B	ÖB	ĜB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	�B	�/B	�5B	�;B	�HB	�ZB	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
%B
%B
+B
DB
VB
\B
bB
hB
oB
oB
oB
uB
{B
�B
�B
"�B
+�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBe`Be`Be`Be`BffBgmBk�Bs�Bt�B�B�DB��B��B��B��B�B��B�B+B�B�BhB
=B��B��B��BB�B%�B#�B�BhB�B�B�B�B=qBT�Be`B�B}�Bo�Bv�B�DB�JB�%Bx�B�Bq�BdZBffBjB_;BP�BH�BC�B@�B/B"�BVB  B�B�HB�B��B��B�3B��B��B��B�VB~�Bx�Bm�BXBP�B6FB"�B �B�B+B
��B
�B
�mB
�#B
ŢB
ÖB
B
��B
�9B
�B
��B
�\B
x�B
^5B
W
B
P�B
H�B
=qB
0!B
�B
!�B
�B
hB
	7B
B	��B	�B	�B	�B	�B	�B	�!B	��B	��B	�B	iyB	bNB	[#B	W
B	VB	O�B	E�B	?}B	5?B	,B	&�B	�B	�B	DB	
=B	+B	B��B��B��B�B�TB�;B�B��B�^B�RB�RB�XB�RB�9B�B�B�'B�-B�B��B��B��B�{B�{B�{B��B��B��B�uB�oB�bB�VB�JB�+B�B�B}�B{�Bw�Bt�Bq�Bo�Bl�Bm�Bm�Bo�Bp�Bq�Bq�Bl�B`BB]/BbNBp�Bk�BffB`BB^5B\)B[#B[#B\)B]/B]/B]/B\)B^5B^5B_;B_;B^5BaHBffBffBffBffBe`BcTBaHB`BB^5B^5B_;B_;BcTBffBgmBhsBhsBjBk�Bk�Bo�Bq�Bp�Bo�Bq�Bp�Bq�Bq�Br�Bu�B{�B~�B�B�+B�7B�DB�uB��B��B��B��B��B��B��B��B�!B�9B�LB�XB�^B�^B�jB�dB�dB�qB�jB�jB�jB�wB�}BBŢBȴBȴBɺB��B��B�B�/B�BB�TB�sB�yB�yB�B�B�B�B�B�fB�TB�B��B	PB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	#�B	$�B	'�B	/B	2-B	33B	8RB	=qB	B�B	D�B	E�B	G�B	J�B	J�B	I�B	L�B	P�B	S�B	W
B	^5B	`BB	aHB	cTB	e`B	gmB	iyB	k�B	o�B	r�B	t�B	y�B	}�B	}�B	|�B	|�B	{�B	|�B	}�B	�B	�B	�%B	�%B	�=B	�\B	�bB	�oB	�oB	�oB	�uB	�hB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�9B	�FB	�FB	�LB	�dB	��B	��B	��B	B	B	B	��B	��B	��B	ÖB	ĜB	ƨB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	�B	�/B	�5B	�;B	�HB	�ZB	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
  B
  B
B
B
B
B
B
%B
%B
%B
%B
B
B
B
%B
%B
+B
DB
VB
\B
bB
hB
oB
oB
oB
uB
{B
�B
�B
"�B
+�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140847                              AO  ARCAADJP                                                                    20181024140847    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140847  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140847  QCF$                G�O�G�O�G�O�0               