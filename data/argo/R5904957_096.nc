CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:21Z creation      
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \x   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^l   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �0   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140821  20181024140821  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               `A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�ǥFZU�1   @�ǥ�b�\@3����S��c�S���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      `A   A   A   @�ff@�  A   A   A@  A`  A�  A�33A�33A�  A�  A�  A�  A�  B   B  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  Bx  B�  B�  B�  B���B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C!�fC$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC]�fC`  Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C��C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C��C��C�  C��3C��3C��3C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��D	� D
  D
� D  D� DfD�fDfD� D��Dy�D  D� D  D� D  D� D  D� DfD� D  D�fD  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$�fD%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D*��D+y�D,  D,� D-fD-� D-��D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ�fDK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ�fDR  DR� DS  DSy�DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D^��D_� D`  D`� DafDa� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dk��Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dq��Dr� Ds  Ds�fDtfDt� Dt��Duy�Dv  Dv� Dw  Dw� Dw��Dy� D�:�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z@ǮA�
A#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B��B��B\)B ��B(��B0��B8��B@��BH��BP��BX��Ba\)Bi\)Bp��Bx��B�z�B�z�B�z�B�G�B�z�B�z�B��B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�B�z�B�z�B�z�B��B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"#�C$=qC&=qC(=qC*=qC,=qC.=qC0WC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCP=qCR=qCT=qCV=qCX=qCZ=qC\#�C^#�C`=qCb#�Cd#�Cf=qCh=qCj=qCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D�D��D	�D	�\D
\D
�\D\D�\D�D��D�D�\D�D��D\D�\D\D�\D\D�\D\D�\D�D�\D\D��D\D��D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D�D�\D\D�\D\D�\D \D �\D!\D!��D"\D"�\D#\D#�\D$\D$��D%\D%�\D&\D&�\D'\D'��D(\D(�\D)\D)�\D*\D*�\D+�D+��D,\D,�\D-�D-�\D.�D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3�D3�\D4\D4�\D5\D5�\D6\D6�\D7\D7��D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=�\D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG�\DH\DH�\DI\DI�\DJ\DJ��DK\DK�\DL\DL�\DM\DM�\DN\DN�\DO\DO�\DP\DP�\DQ\DQ��DR\DR�\DS\DS��DT�DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^�\D_�D_�\D`\D`�\Da�Da�\Db\Db�\Dc\Dc�\Dd\Dd�\De\De�\Df\Df�\Dg\Dg�\Dh\Dh�\Di\Di�\Dj\Dj�\Dk\Dk�\Dl�Dl��Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp�\Dq\Dq�\Dr�Dr�\Ds\Ds��Dt�Dt�\Du�Du��Dv\Dv�\Dw\Dw�\Dw�)Dy�\D�B�D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�t�A�t�A�r�A�t�A�v�A�z�A�z�A�z�A�|�A�|�AށAޅAޅAޅAއ+Aމ7Aއ+AޅAޅA�~�A�O�A�?}A��AݮA�x�A��AؑhA�ffA�ȴAсA�7LA�JAЍPAΙ�A��#AǋDAư!A�+A�p�A�1'A�;dA�p�A�1A�t�A�`BA�oA���A��A���A�oA���A�{A��\A���A���A��9A��A���A�|�A�~�A��A�x�A�%A���A�O�A��7A�z�A�5?A��A��A���A���A���A�z�A���A���A�(�A�dZA���A�-A�A�/A��-A�jA�  A�ZA�Q�A�C�A�JA�C�A�7LA���A��A��^A�ƨA��
A�/A��yA~ĜA|bNAz�yAx�9Awt�Av��Au&�AlJAhĜAfbNAd�DAbn�A`��A^�DA]��A]x�A\^5A[�PA[XA[�AZAYC�AW�^AUS�ATbNAS��ASp�AR�+AQ�^AQ33AP��AO�;ANbNAK��AH�yAF�+AE�AE�AD�AA\)A=�-A<�!A<Q�A;|�A:jA8�\A8{A7�A6�jA5��A3A3&�A25?A0^5A/�;A.�A.bNA.�A+K�A*1A)�A)x�A(�9A'|�A&�jA$��A"�HA"�A!A!VA ffA�PA��AjA��A�A�A�wA�DA�
AC�A��A��A��A�A��A�A�hA�7Ap�AG�A�A��Ap�AXAXAG�A(�AE�A�
A"�A��A
(�A	p�AȴA��AdZA ��A n�@��F@��y@�A�@�n�@�1@��\@�I�@�%@���@��@�ƨ@�S�@�{@��@��@�E�@�r�@�-@݉7@��@ە�@��@���@��@ڟ�@�$�@� �@�$�@�p�@ԣ�@�Z@��
@ӝ�@�o@ҏ\@�`B@�1@�|�@�l�@��y@���@�r�@��@�ȴ@�7L@��
@Ɨ�@�@�`B@ēu@��H@���@�S�@��F@��@���@�~�@���@�n�@�$�@��#@�@�x�@��@��@��9@�bN@���@��@���@��@�z�@�I�@�dZ@��\@��R@��@�C�@�+@���@��@���@��@���@�bN@�Z@�1'@��@��/@�ƨ@���@���@��P@�K�@�o@��@�ȴ@�@�/@��@��@��`@��@��@���@���@���@��@���@�z�@���@�|�@�+@�ff@�5?@�-@���@���@���@���@�x�@�`B@�O�@�7L@�V@��@��j@��@�r�@�1'@���@���@��P@�t�@�\)@��y@�~�@�V@�$�@���@��#@��@��`@��/@��`@��/@��9@���@���@��@�Ĝ@��9@���@�bN@�1@��F@���@��@�l�@�dZ@�;d@��H@��@�ȴ@��!@�=q@�@��^@���@��h@�x�@�X@�/@��@���@��`@��j@���@���@��w@���@�l�@��@���@��!@���@���@��+@���@�/@��@�%@��@��j@�bN@�9X@� �@���@��@��7@��9@�1@��;@�t�@���@�@���@��^@��@�9X@�|�@�C�@��@�E�@���@��-@��h@��@�O�@�/@�7L@�?}@�?}@�7L@�&�@��@���@�Ĝ@��D@�(�@��@��@��@�b@���@�K�@��\@��T@��@�`B@�7L@��9@�bN@��@�dZ@�o@���@���@�M�@�{@��@���@�`B@�O�@�?}@�/@���@���@�A�@�\)@��@�^5@�V@�M�@�M�@�ff@�E�@�{@��T@��T@��#@��-@��7@�X@�X@�X@�&�@�j@��@�\)@�+@�ȴ@��!@�-@��#@���@��h@�x�@��@u�H@bL0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�t�A�t�A�r�A�t�A�v�A�z�A�z�A�z�A�|�A�|�AށAޅAޅAޅAއ+Aމ7Aއ+AޅAޅA�~�A�O�A�?}A��AݮA�x�A��AؑhA�ffA�ȴAсA�7LA�JAЍPAΙ�A��#AǋDAư!A�+A�p�A�1'A�;dA�p�A�1A�t�A�`BA�oA���A��A���A�oA���A�{A��\A���A���A��9A��A���A�|�A�~�A��A�x�A�%A���A�O�A��7A�z�A�5?A��A��A���A���A���A�z�A���A���A�(�A�dZA���A�-A�A�/A��-A�jA�  A�ZA�Q�A�C�A�JA�C�A�7LA���A��A��^A�ƨA��
A�/A��yA~ĜA|bNAz�yAx�9Awt�Av��Au&�AlJAhĜAfbNAd�DAbn�A`��A^�DA]��A]x�A\^5A[�PA[XA[�AZAYC�AW�^AUS�ATbNAS��ASp�AR�+AQ�^AQ33AP��AO�;ANbNAK��AH�yAF�+AE�AE�AD�AA\)A=�-A<�!A<Q�A;|�A:jA8�\A8{A7�A6�jA5��A3A3&�A25?A0^5A/�;A.�A.bNA.�A+K�A*1A)�A)x�A(�9A'|�A&�jA$��A"�HA"�A!A!VA ffA�PA��AjA��A�A�A�wA�DA�
AC�A��A��A��A�A��A�A�hA�7Ap�AG�A�A��Ap�AXAXAG�A(�AE�A�
A"�A��A
(�A	p�AȴA��AdZA ��A n�@��F@��y@�A�@�n�@�1@��\@�I�@�%@���@��@�ƨ@�S�@�{@��@��@�E�@�r�@�-@݉7@��@ە�@��@���@��@ڟ�@�$�@� �@�$�@�p�@ԣ�@�Z@��
@ӝ�@�o@ҏ\@�`B@�1@�|�@�l�@��y@���@�r�@��@�ȴ@�7L@��
@Ɨ�@�@�`B@ēu@��H@���@�S�@��F@��@���@�~�@���@�n�@�$�@��#@�@�x�@��@��@��9@�bN@���@��@���@��@�z�@�I�@�dZ@��\@��R@��@�C�@�+@���@��@���@��@���@�bN@�Z@�1'@��@��/@�ƨ@���@���@��P@�K�@�o@��@�ȴ@�@�/@��@��@��`@��@��@���@���@���@��@���@�z�@���@�|�@�+@�ff@�5?@�-@���@���@���@���@�x�@�`B@�O�@�7L@�V@��@��j@��@�r�@�1'@���@���@��P@�t�@�\)@��y@�~�@�V@�$�@���@��#@��@��`@��/@��`@��/@��9@���@���@��@�Ĝ@��9@���@�bN@�1@��F@���@��@�l�@�dZ@�;d@��H@��@�ȴ@��!@�=q@�@��^@���@��h@�x�@�X@�/@��@���@��`@��j@���@���@��w@���@�l�@��@���@��!@���@���@��+@���@�/@��@�%@��@��j@�bN@�9X@� �@���@��@��7@��9@�1@��;@�t�@���@�@���@��^@��@�9X@�|�@�C�@��@�E�@���@��-@��h@��@�O�@�/@�7L@�?}@�?}@�7L@�&�@��@���@�Ĝ@��D@�(�@��@��@��@�b@���@�K�@��\@��T@��@�`B@�7L@��9@�bN@��@�dZ@�o@���@���@�M�@�{@��@���@�`B@�O�@�?}@�/@���@���@�A�@�\)@��@�^5@�V@�M�@�M�@�ff@�E�@�{@��T@��T@��#@��-@��7@�X@�X@�X@�&�@�j@��@�\)@�+@�ȴ@��!@�-@��#@���@��h@�x�@��@u�H@bL0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BB�BA�BA�BA�BB�BB�BB�BB�BC�BD�BE�BJ�BS�BT�B8RB8RB]/B_;B`BB_;BW
Bk�B��B�qBƨB��BBgmBt�BiyBM�B�B�B�B)�B:^BM�BN�BS�Bv�B{�By�B�+B�uB��B��B��B��B��B��B��B��B��B�VB�Bu�Br�B~�B�%Bm�B=qB��B�B�B�B�fB�B��B��B�Bz�BbNBA�B1B
��B
��B
�HB
�3B
��B
�uB
�7B
�uB
��B
��B
��B
�B
O�B
A�B
:^B
'�B
�B
�B
%B	ŢB	��B	�B	k�B	M�B	8RB	5?B	K�B	O�B	Q�B	P�B	Q�B	P�B	K�B	E�B	@�B	9XB	5?B	49B	33B	0!B	,B	(�B	%�B	!�B	�B	DB��B�`B�5B�/B�B��BĜBÖBB��B�jB�dB�^B�RB�?B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�VB�=B�+B�%B�B�B�B�B}�By�Bx�By�By�Bw�Bw�Bx�Bz�B{�B~�B�B�%B�=B�PB�VB�VB�VB�VB�bB�hB�oB�oB�oB�oB�uB�uB�hB�VB�DB�1B�Bx�Bv�Bs�Bq�Bo�Bn�Bn�Bk�BiyBiyBl�Bx�B�B�=B�VB�bB��B��B��B��B��B�B�-B�9B�RB�^B�dB�dB�jB�wBŢB��B��B��B��B��B��B��B��B��B�B�)B�)B�)B�)B�B�B�B�B�B�
B�
B�B�
B��B��B��B�B�NB��B��B	  B	  B	  B	  B	  B	B	DB	VB	bB	bB	uB	�B	�B	�B	�B	 �B	&�B	-B	33B	6FB	;dB	?}B	;dB	>wB	E�B	F�B	F�B	H�B	I�B	J�B	\)B	\)B	_;B	`BB	bNB	dZB	e`B	hsB	hsB	iyB	r�B	y�B	{�B	|�B	~�B	~�B	~�B	~�B	~�B	~�B	�B	�B	�1B	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�'B	�'B	�'B	�3B	�FB	�LB	�RB	�XB	�XB	�jB	�wB	�}B	�}B	�}B	�}B	�}B	�}B	��B	��B	��B	��B	��B	B	B	B	B	ĜB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�HB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�mB	�yB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
+B
+B
+B
+B
+B
+B
+B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
	7B
	7B
1B
	7B
	7B

=B
DB
DB
JB
JB
PB
VB
VB
VB
\B
bB
bB
bB
oB
hB
oB
oB
hB
bB
bB
bB
bB
hB
hB
�B
%FB
5�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BB�BA�BA�BA�BB�BB�BB�BB�BC�BD�BE�BJ�BS�BT�B8RB8RB]/B_;B`BB_;BW
Bk�B��B�qBƨB��BBgmBt�BiyBM�B�B�B�B)�B:^BM�BN�BS�Bv�B{�By�B�+B�uB��B��B��B��B��B��B��B��B��B�VB�Bu�Br�B~�B�%Bm�B=qB��B�B�B�B�fB�B��B��B�Bz�BbNBA�B1B
��B
��B
�HB
�3B
��B
�uB
�7B
�uB
��B
��B
��B
�B
O�B
A�B
:^B
'�B
�B
�B
%B	ŢB	��B	�B	k�B	M�B	8RB	5?B	K�B	O�B	Q�B	P�B	Q�B	P�B	K�B	E�B	@�B	9XB	5?B	49B	33B	0!B	,B	(�B	%�B	!�B	�B	DB��B�`B�5B�/B�B��BĜBÖBB��B�jB�dB�^B�RB�?B�!B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�VB�=B�+B�%B�B�B�B�B}�By�Bx�By�By�Bw�Bw�Bx�Bz�B{�B~�B�B�%B�=B�PB�VB�VB�VB�VB�bB�hB�oB�oB�oB�oB�uB�uB�hB�VB�DB�1B�Bx�Bv�Bs�Bq�Bo�Bn�Bn�Bk�BiyBiyBl�Bx�B�B�=B�VB�bB��B��B��B��B��B�B�-B�9B�RB�^B�dB�dB�jB�wBŢB��B��B��B��B��B��B��B��B��B�B�)B�)B�)B�)B�B�B�B�B�B�
B�
B�B�
B��B��B��B�B�NB��B��B	  B	  B	  B	  B	  B	B	DB	VB	bB	bB	uB	�B	�B	�B	�B	 �B	&�B	-B	33B	6FB	;dB	?}B	;dB	>wB	E�B	F�B	F�B	H�B	I�B	J�B	\)B	\)B	_;B	`BB	bNB	dZB	e`B	hsB	hsB	iyB	r�B	y�B	{�B	|�B	~�B	~�B	~�B	~�B	~�B	~�B	�B	�B	�1B	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�'B	�'B	�'B	�'B	�3B	�FB	�LB	�RB	�XB	�XB	�jB	�wB	�}B	�}B	�}B	�}B	�}B	�}B	��B	��B	��B	��B	��B	B	B	B	B	ĜB	ȴB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�5B	�;B	�;B	�HB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�mB	�yB	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
+B
+B
+B
+B
+B
+B
+B
1B
	7B

=B

=B

=B

=B

=B
DB
DB
	7B
	7B
1B
	7B
	7B

=B
DB
DB
JB
JB
PB
VB
VB
VB
\B
bB
bB
bB
oB
hB
oB
oB
hB
bB
bB
bB
bB
hB
hB
�B
%FB
5�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140821                              AO  ARCAADJP                                                                    20181024140821    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140821  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140821  QCF$                G�O�G�O�G�O�0               