CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:16Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024140816  20181024140816  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               ?A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׿en,�^1   @׿e��Ќ@2�I�^5�c�XbM�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      ?A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BO��BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���B���C   C  C  C  C  C
  C  C  C�C�C�fC  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � DfD� D  D� D  D� D  D� D��Dy�D  D� DfD� D  D� D	  D	� D
  D
�fD  D�fDfD� D  D� D��D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)fD)�fD*  D*� D+  D+� D,  D,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D9��D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQy�DQ��DR� DS  DS� DS��DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDefDe� Df  Dfy�Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� DvfDv� Dv��Dw� Dx  DxL�Dy�\D�AHD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��G@ǮA�
A#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B��B��B��B ��B(�]B0��B8��B@��BH��BP�]BX�]B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�B�z�B�z�B�z�B�G�B�G�C =qC=qC=qC=qC=qC
=qC=qC=qCWCWC#�C=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<#�C>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\#�C^=qC`=qCb=qCd#�Cf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C�+�C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�D \D �\D�D�\D\D�\D\D�\D\D�\D�D��D\D�\D�D�\D\D�\D	\D	�\D
\D
��D\D��D�D�\D\D�\D�D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(��D)�D)��D*\D*�\D+\D+�\D,\D,��D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:�D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA��DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP�DP�\DQ\DQ��DR�DR�\DS\DS�\DT�DT��DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY��DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd��De�De�\Df\Df��Dg\Dg�\Dh\Dh�\Di\Di�\Dj�Dj�\Dk\Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp�Dp�\Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt\Dt�\Du\Du�\Dv�Dv�\Dw�Dw�\Dx\Dx\)DyθD�H�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ĜA���A���A���A���A���A��
A��/A��HA���A��/A��`A��`A��`A��mA��mA��mA��HA��TA��A��mA�AۼjAۼjAۏ\A��A��A٣�A�M�A��A�+A�33A�5?A�~�A��A�=qA�&�A�?}A�p�A�5?AׅA֟�A�|�A�t�AӺ^A�ĜA��AѶFA��/A�$�AΣ�A�\)A��yA�1'A��A�;dA�bNAɁA�;dA�?}AŮA�XA�|�A�/A¶FA�t�A���A�S�A��A�ZA��`A�K�A���A��wA�O�A���A�O�A�E�A�~�A���A��\A�hsA���A��FA�x�A��yA��DA��/A��A�A�(�A���A�C�A�\)A�E�A��A�A�t�A���A�E�A�A�K�A�l�A�G�A�A�A���A���A�l�A�oA�
=A�r�A�n�A�-A��A�33A|$�Az�yAy|�Au�
As�Arz�ApjAm��AjA�AhVAf��Ad�DAb�`A^�A\I�AZ�AVAS��APM�AOK�AMK�AL=qAKAJ��AI&�AFffAEXAD�AC��AC�ABn�ABbAAK�A?`BA=��A=+A<�A:��A9hsA7G�A6�A6{A4bNA1t�A0��A0M�A0$�A/��A-�mA-hsA,�A,  A+;dA*(�A(�A'�A'ƨA'ƨA'A'�A$��A#?}A A�AoA�A�AM�A�A�#A�TAn�A%A�RAv�A�;A"�A��A  AZAQ�A��A^5A��A��A�AbA�hA�A��AffA9XA�A�mAl�Ar�A$�A�AG�A=qA
��A
v�A
1A	��A��A�A��A��A��A Z@���@�E�@��#@���@��j@�z�@��H@���@�V@��@��;@�E�@�dZ@�{@���@��@�x�@�(�@�|�@�M�@��T@�j@��@�{@�`B@ܣ�@�9X@��
@�K�@ڟ�@�5?@�@���@��@ם�@ָR@�`B@ԃ@�K�@���@�7L@�@���@��@�~�@͡�@�`B@��@���@�z�@��@ˮ@��
@��T@Õ�@�C�@���@��@�1'@� �@�\)@+@��T@��\@�M�@��@�{@�ff@��m@�=q@�E�@��@���@�9X@� �@�  @��@���@�`B@�%@���@�1'@���@��P@�;d@���@��R@�ff@�=q@��@���@�%@��@�9X@�ƨ@�C�@��y@���@�{@�p�@�7L@��@��/@���@�j@��;@�dZ@�+@���@��!@���@��/@�r�@�Z@�Q�@�I�@�A�@� �@��@�t�@�C�@�@��R@��+@�E�@���@���@�hs@�G�@�V@���@���@�9X@�1@��@�t�@�33@��H@��\@��\@�ff@�{@��T@��-@��@�&�@���@�bN@�A�@�(�@��@�b@�b@�1@��@��y@���@�M�@�-@���@�V@�z�@�  @��F@�"�@��@��+@�G�@�V@���@�A�@�b@�A�@�A�@�Q�@�I�@��;@��y@�n�@�J@��@�$�@�M�@�{@���@��^@��^@��7@�O�@�Ĝ@���@�r�@�I�@�A�@�1'@�b@���@���@�C�@�"�@��@�M�@��@���@�hs@�7L@���@�Z@�1'@�ƨ@���@��@�t�@�dZ@�S�@�33@�ȴ@��+@�@���@��@�&�@�%@�%@���@���@�z�@�1@���@�S�@�=q@���@�7L@�&�@��/@�Ĝ@�Ĝ@��9@��D@�j@�Z@�b@��
@�C�@�~�@�M�@�@��^@���@�?}@�V@�%@�%@���@��@��@�9X@�t�@��!@���@��@��7@�/@��@���@��D@�z�@�r�@�bN@� �@�ƨ@��@��@r��@a/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ĜA���A���A���A���A���A��
A��/A��HA���A��/A��`A��`A��`A��mA��mA��mA��HA��TA��A��mA�AۼjAۼjAۏ\A��A��A٣�A�M�A��A�+A�33A�5?A�~�A��A�=qA�&�A�?}A�p�A�5?AׅA֟�A�|�A�t�AӺ^A�ĜA��AѶFA��/A�$�AΣ�A�\)A��yA�1'A��A�;dA�bNAɁA�;dA�?}AŮA�XA�|�A�/A¶FA�t�A���A�S�A��A�ZA��`A�K�A���A��wA�O�A���A�O�A�E�A�~�A���A��\A�hsA���A��FA�x�A��yA��DA��/A��A�A�(�A���A�C�A�\)A�E�A��A�A�t�A���A�E�A�A�K�A�l�A�G�A�A�A���A���A�l�A�oA�
=A�r�A�n�A�-A��A�33A|$�Az�yAy|�Au�
As�Arz�ApjAm��AjA�AhVAf��Ad�DAb�`A^�A\I�AZ�AVAS��APM�AOK�AMK�AL=qAKAJ��AI&�AFffAEXAD�AC��AC�ABn�ABbAAK�A?`BA=��A=+A<�A:��A9hsA7G�A6�A6{A4bNA1t�A0��A0M�A0$�A/��A-�mA-hsA,�A,  A+;dA*(�A(�A'�A'ƨA'ƨA'A'�A$��A#?}A A�AoA�A�AM�A�A�#A�TAn�A%A�RAv�A�;A"�A��A  AZAQ�A��A^5A��A��A�AbA�hA�A��AffA9XA�A�mAl�Ar�A$�A�AG�A=qA
��A
v�A
1A	��A��A�A��A��A��A Z@���@�E�@��#@���@��j@�z�@��H@���@�V@��@��;@�E�@�dZ@�{@���@��@�x�@�(�@�|�@�M�@��T@�j@��@�{@�`B@ܣ�@�9X@��
@�K�@ڟ�@�5?@�@���@��@ם�@ָR@�`B@ԃ@�K�@���@�7L@�@���@��@�~�@͡�@�`B@��@���@�z�@��@ˮ@��
@��T@Õ�@�C�@���@��@�1'@� �@�\)@+@��T@��\@�M�@��@�{@�ff@��m@�=q@�E�@��@���@�9X@� �@�  @��@���@�`B@�%@���@�1'@���@��P@�;d@���@��R@�ff@�=q@��@���@�%@��@�9X@�ƨ@�C�@��y@���@�{@�p�@�7L@��@��/@���@�j@��;@�dZ@�+@���@��!@���@��/@�r�@�Z@�Q�@�I�@�A�@� �@��@�t�@�C�@�@��R@��+@�E�@���@���@�hs@�G�@�V@���@���@�9X@�1@��@�t�@�33@��H@��\@��\@�ff@�{@��T@��-@��@�&�@���@�bN@�A�@�(�@��@�b@�b@�1@��@��y@���@�M�@�-@���@�V@�z�@�  @��F@�"�@��@��+@�G�@�V@���@�A�@�b@�A�@�A�@�Q�@�I�@��;@��y@�n�@�J@��@�$�@�M�@�{@���@��^@��^@��7@�O�@�Ĝ@���@�r�@�I�@�A�@�1'@�b@���@���@�C�@�"�@��@�M�@��@���@�hs@�7L@���@�Z@�1'@�ƨ@���@��@�t�@�dZ@�S�@�33@�ȴ@��+@�@���@��@�&�@�%@�%@���@���@�z�@�1@���@�S�@�=q@���@�7L@�&�@��/@�Ĝ@�Ĝ@��9@��D@�j@�Z@�b@��
@�C�@�~�@�M�@�@��^@���@�?}@�V@�%@�%@���@��@��@�9X@�t�@��!@���@��@��7@�/@��@���@��D@�z�@�r�@�bN@� �@�ƨ@��@��@r��@a/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
M�B
L�B
I�B
<jB
0!B
,B
49B
9XB
YB
dZB
o�B
�oB
�dB
�B
�B
�LB
ɺB
�B
�5B
�BB
�B�BD�BE�BO�Bv�Bz�B�PB��B��B��B��B�B�3B�B�B
=B<jBP�B]/Bp�B�7B�\B��B�B�RB�?B��B��B��B�7BXB8RB$�B�B�B\BB�B�B�ZB�)B�B��BƨB�B��Bt�Bp�Bx�B�B�1BdZBT�BO�BH�BA�B8RB$�BPB  B
�B
�HB
�}B
�'B
�B
��B
�DB
�B
u�B
p�B
iyB
_;B
D�B
9XB
.B
�B

=B	��B	�B	�B	ƨB	�XB	�B	��B	��B	�B	o�B	cTB	J�B	;dB	(�B	"�B	�B	hB	PB	1B	B��B�B�B�B�sB�fB�ZB�BB�B��B��B��BȴBB�qB�dB�XB�9B�B�B�B�B�B��B��B��B��B��B��B��B�{B�{B�{B�uB�hB�DB�B�B�B� B~�B}�B|�B{�B�%B��B�JB�7B�%B~�Bz�By�B��B�B�-B�3B�RB�LB�FB�?B�FB�LB�RB�^B�dB�dB�dB�dB�jB�wB�wB�wB�qB�dB�XB�RB�RB�RB�XB�^B�XB�LB�FB�FB�LB�LB�FB�RB�FB�9B�3B�-B�-B�9B�dB�jB�dB�XB�FB�3B�!B�'B�LB�RB�^BƨB��B��B��B��B��B�
B�B�B�#B�/B�;B�TB�TB�`B�mB�sB�yB�sB�`B�5B�BB�B��B��B��B��B��B��B��B��B	B��B�B�B�B�B	+B	DB	+B	B�B�#B�#B�
B��B�#B	B	hB	�B	�B	�B	�B	�B	�B	"�B	(�B	-B	/B	2-B	5?B	7LB	;dB	?}B	@�B	B�B	E�B	F�B	I�B	K�B	O�B	Q�B	W
B	ZB	]/B	`BB	bNB	e`B	jB	k�B	l�B	m�B	o�B	p�B	t�B	v�B	x�B	z�B	~�B	�B	�+B	�1B	�1B	�1B	�1B	�1B	�1B	�=B	�JB	�\B	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�9B	�RB	�XB	�^B	�dB	�dB	�}B	��B	ŢB	ŢB	ĜB	ÖB	��B	��B	��B	��B	B	ÖB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�NB	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
+B
+B
+B
+B
+B
%B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B

=B
JB
VB
VB
bB
oB
oB
oB
oB
oB
oB
uB
uB
uB
{B
{B
�B
%�B
4�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
M�B
M�B
L�B
I�B
<jB
0!B
,B
49B
9XB
YB
dZB
o�B
�oB
�dB
�B
�B
�LB
ɺB
�B
�5B
�BB
�B�BD�BE�BO�Bv�Bz�B�PB��B��B��B��B�B�3B�B�B
=B<jBP�B]/Bp�B�7B�\B��B�B�RB�?B��B��B��B�7BXB8RB$�B�B�B\BB�B�B�ZB�)B�B��BƨB�B��Bt�Bp�Bx�B�B�1BdZBT�BO�BH�BA�B8RB$�BPB  B
�B
�HB
�}B
�'B
�B
��B
�DB
�B
u�B
p�B
iyB
_;B
D�B
9XB
.B
�B

=B	��B	�B	�B	ƨB	�XB	�B	��B	��B	�B	o�B	cTB	J�B	;dB	(�B	"�B	�B	hB	PB	1B	B��B�B�B�B�sB�fB�ZB�BB�B��B��B��BȴBB�qB�dB�XB�9B�B�B�B�B�B��B��B��B��B��B��B��B�{B�{B�{B�uB�hB�DB�B�B�B� B~�B}�B|�B{�B�%B��B�JB�7B�%B~�Bz�By�B��B�B�-B�3B�RB�LB�FB�?B�FB�LB�RB�^B�dB�dB�dB�dB�jB�wB�wB�wB�qB�dB�XB�RB�RB�RB�XB�^B�XB�LB�FB�FB�LB�LB�FB�RB�FB�9B�3B�-B�-B�9B�dB�jB�dB�XB�FB�3B�!B�'B�LB�RB�^BƨB��B��B��B��B��B�
B�B�B�#B�/B�;B�TB�TB�`B�mB�sB�yB�sB�`B�5B�BB�B��B��B��B��B��B��B��B��B	B��B�B�B�B�B	+B	DB	+B	B�B�#B�#B�
B��B�#B	B	hB	�B	�B	�B	�B	�B	�B	"�B	(�B	-B	/B	2-B	5?B	7LB	;dB	?}B	@�B	B�B	E�B	F�B	I�B	K�B	O�B	Q�B	W
B	ZB	]/B	`BB	bNB	e`B	jB	k�B	l�B	m�B	o�B	p�B	t�B	v�B	x�B	z�B	~�B	�B	�+B	�1B	�1B	�1B	�1B	�1B	�1B	�=B	�JB	�\B	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�'B	�'B	�9B	�RB	�XB	�^B	�dB	�dB	�}B	��B	ŢB	ŢB	ĜB	ÖB	��B	��B	��B	��B	B	ÖB	ƨB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�#B	�)B	�/B	�NB	�ZB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
+B
+B
+B
+B
+B
+B
%B
B
B
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B

=B
JB
VB
VB
bB
oB
oB
oB
oB
oB
oB
uB
uB
uB
{B
{B
�B
%�B
4�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140816                              AO  ARCAADJP                                                                    20181024140816    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140816  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140816  QCF$                G�O�G�O�G�O�0               