CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:15Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140815  20181024140815  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               <A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׾���01   @׾�s���@2���`A��c��t�j1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      <A   A   A   @�  @�  @���A   A@  A`  A�  A�33A�33A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC'�fC*  C,  C.�C0�C2  C3�fC5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCO�fCR  CT  CV  CX  CZ  C\  C^  C_�fCa�fCd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C��C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C��3C�  C��C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3D   D � D  D� D  D� DfD� D  D� D  Dy�D  D� D  D� D  D� D	  D	y�D
  D
� D  D� DfD� D��D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� DfD� D   D � D ��D!� D"  D"�fD#  D#� D$  D$� D%  D%y�D%��D&y�D'  D'y�D'��D(� D)  D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D0��D1� D2  D2� D3  D3� D3��D4y�D4��D5y�D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� D@��DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDTfDT� DU  DU� DVfDV�fDWfDW� DXfDX�fDY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dl��Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr� Ds  Ds� Dt  Dty�Du  Du� Dv  Dv� DwfDw�fDw��Dy�)D�0RD�˅1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@ǮA=qA#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B��B��B�]B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC#�C=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&#�C(#�C*=qC,=qC.WC0WC2=qC4#�C6#�C8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN#�CP#�CR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`#�Cb#�Cd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C�+�C�+�C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D�D�\D\D�\D\D��D\D�\D\D�\D\D�\D	\D	��D
\D
�\D\D�\D�D�\D�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D�D�\D \D �\D!�D!�\D"\D"��D#\D#�\D$\D$�\D%\D%��D&�D&��D'\D'��D(�D(�\D)\D)��D*\D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1�D1�\D2\D2�\D3\D3�\D4�D4��D5�D5��D6\D6�\D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA�DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ�\DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS��DT�DT�\DU\DU�\DV�DV��DW�DW�\DX�DX��DY\DY�\DZ\DZ��D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_\D_�\D`\D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj��Dk\Dk�\Dl\Dl�\Dm�Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr�Dr�\Ds\Ds�\Dt\Dt��Du\Du�\Dv\Dv�\Dw�Dw��Dw�)Dy��D�8 D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�r�A�z�A�z�A�z�A�z�A�z�A�|�A�n�A�7LA�"�A�bA�A���A���A��`A��/A���A���A�ƨAݾwAݸRAݰ!Aݏ\A݁A�dZA��A��TAܩ�A܁A�"�A�z�Aɡ�A�;dAƼjAƁA�^5A� �A��Aš�A�&�AĴ9Aô9A��`A��!A�9XA���A��A��DA�x�A�A�z�A���A�Q�A�VA��A�\)A��`A� �A��A�1'A�|�A��uA�ȴA� �A�p�A���A�/A���A�G�A�VA���A��;A�jA�"�A�A�ȴA�bA���A��A�;dA��A���A��yA���A�bNA�C�A�%A��^A���A�VA�p�A��;A�A�A{�Av1'Ar��AqVAn�Amp�Al��Ak/AiG�Agt�Ae��Ad��Ab��A`ĜA`  A_l�A^M�A[��AZ�HAZQ�AW��AU�AS7LAP�/AN�RAMx�AK�AJ��AH�RAG�AF��AE\)AD^5AB��A@��A@bA?O�A>z�A>�A=��A;A:�A8�9A7��A6��A5"�A2=qA1�A0ffA.��A.A-�wA,ȴA*�!A)�^A)A(ffA'�
A'hsA&��A&I�A%33A$Q�A#�
A#hsA"5?A!��A �uAl�AK�A�#A�/A��A�A��A�/A=qA�^Av�A�9A
=A�HAM�A�^A��AdZA�AI�A7LA�AG�AC�A�DA�A�A��A
��A
bA�yA��A�RA�A��A"�A ~�A ~�A ��@���@��y@���@���@�9X@�|�@�$�@�Z@�@�r�@�dZ@���@�X@�bN@���@��;@�&�@�l�@�n�@�@�w@�"�@�ȴ@�v�@�`B@���@�9X@�K�@�"�@���@ޗ�@�E�@ݑh@��@�bN@ڗ�@�X@��@�9X@�V@ϥ�@��@��@�l�@���@���@�O�@�l�@���@��@�I�@�9X@��
@��`@�%@öF@��@ư!@�ȴ@�ff@ŉ7@��`@ě�@� �@�|�@��-@��@��`@���@�1@��@�+@�$�@���@��@��@�1'@��m@���@�dZ@�C�@�"�@�n�@�@��^@���@��-@���@�O�@���@��/@��9@��@�Z@�9X@���@�t�@�t�@�\)@�C�@�
=@�ȴ@��!@���@�5?@�J@��T@���@���@���@�Z@�1'@��@��@��
@��w@��@�\)@��@���@���@��+@�J@��^@�O�@��9@�bN@�1'@� �@�b@�1@��@��@�ff@��@���@�p�@�/@���@��@��@��F@�dZ@���@��\@�=q@��@��^@��-@�`B@�7L@���@���@�j@�ƨ@�S�@�@��H@���@�v�@�V@�E�@�5?@�J@��T@���@��7@�X@���@�A�@�  @��@��
@�t�@�
=@��@��!@�~�@�5?@��@���@�Ĝ@�bN@��w@���@���@���@��^@��@�9X@�9X@�~�@�$�@�{@�J@�@���@��@���@�p�@��/@�z�@��u@��@�I�@�Z@�bN@�Z@�Q�@�I�@�9X@�1@��m@�K�@��@�ȴ@��+@���@��@��-@��@�@��T@�@��h@���@���@�9X@��@��P@�dZ@�;d@�+@�ȴ@�^5@�{@��T@��h@���@�r�@�(�@��w@�S�@�33@��@���@�^5@�$�@��@��7@�hs@�O�@�G�@�?}@�7L@�/@�/@��@�A�@�1@��m@��
@��@�t�@���@�E�@�5?@�{@�$�@��#@���@��@���@y��@f��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�r�A�z�A�z�A�z�A�z�A�z�A�|�A�n�A�7LA�"�A�bA�A���A���A��`A��/A���A���A�ƨAݾwAݸRAݰ!Aݏ\A݁A�dZA��A��TAܩ�A܁A�"�A�z�Aɡ�A�;dAƼjAƁA�^5A� �A��Aš�A�&�AĴ9Aô9A��`A��!A�9XA���A��A��DA�x�A�A�z�A���A�Q�A�VA��A�\)A��`A� �A��A�1'A�|�A��uA�ȴA� �A�p�A���A�/A���A�G�A�VA���A��;A�jA�"�A�A�ȴA�bA���A��A�;dA��A���A��yA���A�bNA�C�A�%A��^A���A�VA�p�A��;A�A�A{�Av1'Ar��AqVAn�Amp�Al��Ak/AiG�Agt�Ae��Ad��Ab��A`ĜA`  A_l�A^M�A[��AZ�HAZQ�AW��AU�AS7LAP�/AN�RAMx�AK�AJ��AH�RAG�AF��AE\)AD^5AB��A@��A@bA?O�A>z�A>�A=��A;A:�A8�9A7��A6��A5"�A2=qA1�A0ffA.��A.A-�wA,ȴA*�!A)�^A)A(ffA'�
A'hsA&��A&I�A%33A$Q�A#�
A#hsA"5?A!��A �uAl�AK�A�#A�/A��A�A��A�/A=qA�^Av�A�9A
=A�HAM�A�^A��AdZA�AI�A7LA�AG�AC�A�DA�A�A��A
��A
bA�yA��A�RA�A��A"�A ~�A ~�A ��@���@��y@���@���@�9X@�|�@�$�@�Z@�@�r�@�dZ@���@�X@�bN@���@��;@�&�@�l�@�n�@�@�w@�"�@�ȴ@�v�@�`B@���@�9X@�K�@�"�@���@ޗ�@�E�@ݑh@��@�bN@ڗ�@�X@��@�9X@�V@ϥ�@��@��@�l�@���@���@�O�@�l�@���@��@�I�@�9X@��
@��`@�%@öF@��@ư!@�ȴ@�ff@ŉ7@��`@ě�@� �@�|�@��-@��@��`@���@�1@��@�+@�$�@���@��@��@�1'@��m@���@�dZ@�C�@�"�@�n�@�@��^@���@��-@���@�O�@���@��/@��9@��@�Z@�9X@���@�t�@�t�@�\)@�C�@�
=@�ȴ@��!@���@�5?@�J@��T@���@���@���@�Z@�1'@��@��@��
@��w@��@�\)@��@���@���@��+@�J@��^@�O�@��9@�bN@�1'@� �@�b@�1@��@��@�ff@��@���@�p�@�/@���@��@��@��F@�dZ@���@��\@�=q@��@��^@��-@�`B@�7L@���@���@�j@�ƨ@�S�@�@��H@���@�v�@�V@�E�@�5?@�J@��T@���@��7@�X@���@�A�@�  @��@��
@�t�@�
=@��@��!@�~�@�5?@��@���@�Ĝ@�bN@��w@���@���@���@��^@��@�9X@�9X@�~�@�$�@�{@�J@�@���@��@���@�p�@��/@�z�@��u@��@�I�@�Z@�bN@�Z@�Q�@�I�@�9X@�1@��m@�K�@��@�ȴ@��+@���@��@��-@��@�@��T@�@��h@���@���@�9X@��@��P@�dZ@�;d@�+@�ȴ@�^5@�{@��T@��h@���@�r�@�(�@��w@�S�@�33@��@���@�^5@�$�@��@��7@�hs@�O�@�G�@�?}@�7L@�/@�/@��@�A�@�1@��m@��
@��@�t�@���@�E�@�5?@�{@�$�@��#@���@��@���@y��@f��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�5B
�mB
�sB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B  BB�B��B��BBBB1BDBoB$�B6FBD�BP�BhsBo�BZB%�B0!B33B49B1'B,B�BoBDB��B�B�HB�BɺB��B�FB��B�hB�=B�By�BjBZBQ�BS�BiyBk�BhsBm�Br�Bl�BB�B(�B%B
�B
�B
��B
�)B
�B
��B
��B
�wB
�B
��B
�%B
cTB
H�B
'�B

=B	��B	�B	�sB	�BB	�B	��B	��B	�FB	�B	��B	��B	�=B	�B	� B	w�B	jB	dZB	^5B	P�B	F�B	9XB	,B	 �B	�B	oB	JB	B��B��B�B�B�ZB�B�
B��B��B��B��B��BɺB��B�RB�FB�B��B��B��B��B��B��B��B��B��B�uB�oB�bB�\B�VB�PB�DB�JB�PB�VB�uB��B��B��B��B��B��B��B�B�'B�9B�FB�FB�'B��B�dB��B�jB�jB�qB�RB�FB�XB�qB��BÖB��B��B�mB��B��B�B�yB�ZB�)B��BŢBÖB�qB�wB��B�B�
B��B��B��B��B��B��BŢBŢBǮBǮBǮBƨBƨB��B��B�}B�}B��B��BÖBŢBŢBŢBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�DB�VB�JB�PB�bB�bB�uB��B��B��B��B�BɺB�/B�BB�B��B��B��B	B	B	B	+B	+B	
=B	JB	PB	bB	hB	hB	oB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	'�B	+B	-B	.B	-B	.B	1'B	1'B	2-B	33B	49B	49B	:^B	;dB	<jB	=qB	?}B	B�B	E�B	F�B	G�B	H�B	J�B	M�B	O�B	P�B	S�B	VB	W
B	XB	YB	[#B	\)B	]/B	`BB	dZB	e`B	ffB	ffB	hsB	iyB	jB	l�B	n�B	o�B	o�B	o�B	o�B	p�B	u�B	x�B	{�B	~�B	� B	�B	�B	�B	�1B	�=B	�JB	�VB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�?B	�?B	�FB	�RB	�XB	�XB	�^B	�dB	�jB	�}B	��B	B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�)B	�)B	�/B	�5B	�;B	�;B	�BB	�BB	�;B	�5B	�)B	�B	�B	�B	�#B	�#B	�)B	�5B	�TB	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
B
%B
KB
 B
1[1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�#B
�#B
�#B
�#B
�#B
�#B
�#B
�5B
�mB
�sB
�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
��B
��B  BB�B��B��BBBB1BDBoB$�B6FBD�BP�BhsBo�BZB%�B0!B33B49B1'B,B�BoBDB��B�B�HB�BɺB��B�FB��B�hB�=B�By�BjBZBQ�BS�BiyBk�BhsBm�Br�Bl�BB�B(�B%B
�B
�B
��B
�)B
�B
��B
��B
�wB
�B
��B
�%B
cTB
H�B
'�B

=B	��B	�B	�sB	�BB	�B	��B	��B	�FB	�B	��B	��B	�=B	�B	� B	w�B	jB	dZB	^5B	P�B	F�B	9XB	,B	 �B	�B	oB	JB	B��B��B�B�B�ZB�B�
B��B��B��B��B��BɺB��B�RB�FB�B��B��B��B��B��B��B��B��B��B�uB�oB�bB�\B�VB�PB�DB�JB�PB�VB�uB��B��B��B��B��B��B��B�B�'B�9B�FB�FB�'B��B�dB��B�jB�jB�qB�RB�FB�XB�qB��BÖB��B��B�mB��B��B�B�yB�ZB�)B��BŢBÖB�qB�wB��B�B�
B��B��B��B��B��B��BŢBŢBǮBǮBǮBƨBƨB��B��B�}B�}B��B��BÖBŢBŢBŢBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�DB�VB�JB�PB�bB�bB�uB��B��B��B��B�BɺB�/B�BB�B��B��B��B	B	B	B	+B	+B	
=B	JB	PB	bB	hB	hB	oB	{B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	'�B	+B	-B	.B	-B	.B	1'B	1'B	2-B	33B	49B	49B	:^B	;dB	<jB	=qB	?}B	B�B	E�B	F�B	G�B	H�B	J�B	M�B	O�B	P�B	S�B	VB	W
B	XB	YB	[#B	\)B	]/B	`BB	dZB	e`B	ffB	ffB	hsB	iyB	jB	l�B	n�B	o�B	o�B	o�B	o�B	p�B	u�B	x�B	{�B	~�B	� B	�B	�B	�B	�1B	�=B	�JB	�VB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�?B	�?B	�FB	�RB	�XB	�XB	�^B	�dB	�jB	�}B	��B	B	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�#B	�)B	�)B	�/B	�5B	�;B	�;B	�BB	�BB	�;B	�5B	�)B	�B	�B	�B	�#B	�#B	�)B	�5B	�TB	�`B	�fB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
B
%B
KB
 B
1[1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140815                              AO  ARCAADJP                                                                    20181024140815    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140815  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140815  QCF$                G�O�G�O�G�O�0               