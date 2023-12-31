CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:53Z creation      
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
resolution        =���   axis      Z        h  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     h  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  S`   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  mP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     h  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �p   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �p   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190553  20181005190553  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @����2��1   @���UUjF@1�vȴ9�c�7KƧ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @���@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B��B   B(ffB0  B8  B@  BH  BPffBXffB`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C�fC�fC  C�C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D	  D	y�D	��D
y�D  D� D  D�fDfD�fDfD�fDfD� D  D� D  Dy�D  D�fD  D� D  Dy�D  D� D  D� D  D� D  D� DfD� D��D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D!��D"y�D"��D#� D$fD$� D%  D%� D&  D&� D'  D'� D'��D(� D)  D)y�D)��D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4y�D5  D5�fD6fD6�fD7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<y�D=  D=� D=��D>y�D?  D?�fD@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDG  DGy�DH  DH�fDIfDI� DI��DJ� DK  DK� DK��DL� DM  DM� DM��DNy�DO  DO�fDPfDP� DQ  DQ�fDR  DR� DS  DS� DT  DT� DU  DU�fDVfDV� DW  DW� DX  DX� DY  DY�fDZfDZ�fD[  D[�fD\  D\�fD]fD]� D]��D^� D_  D_� D`  D`y�Da  Da� Da��Db� Dc  Dcy�Dd  Dd� De  De� Df  Dfy�Dg  Dgy�Dg��Dh� Di  Di� Dj  Djy�Dj��Dk� Dl  Dl� Dm  Dm� DnfDn� Dn��Do� DpfDp�fDq  Dq� Dr  Dr� Ds  Ds� Ds��Dt� Du  Duy�Dv  Dv� Dw  Dw� Dw�3Dy�fD�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@ǮA�
A#�
AB=pAc�
A��A��A��A��A��A��A��A��B ��B	\)B��B�]B ��B)\)B0��B8��B@��BH��BQ\)BY\)B`��Bh��Bp��Bx��B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�G�B�z�B�z�B��B�z�B�z�B�G�B�z�BĮB�z�B�z�B�z�B�z�B�z�B�z�B�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC#�C#�C=qCWC
=qC=qC=qC=qC=qC=qC=qCWC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCNWCP=qCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCj=qCl=qCn=qCp=qCr=qCt#�Cv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C�+�C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��D \D �\D�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D\D�\D	\D	��D
�D
��D\D�\D\D��D�D��D�D��D�D�\D\D�\D\D��D\D��D\D�\D\D��D\D�\D\D�\D\D�\D\D�\D�D�\D�D�\D\D��D\D�\D\D�\D\D�\D\D�\D \D �\D!\D!�\D"�D"��D#�D#�\D$�D$�\D%\D%�\D&\D&�\D'\D'�\D(�D(�\D)\D)��D*�D*�\D+�D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4��D5\D5��D6�D6��D7\D7�\D8\D8�\D9\D9�\D:\D:�\D;\D;�\D<\D<��D=\D=�\D>�D>��D?\D?��D@\D@�\DA\DA�\DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF��DG\DG��DH\DH��DI�DI�\DJ�DJ�\DK\DK�\DL�DL�\DM\DM�\DN�DN��DO\DO��DP�DP�\DQ\DQ��DR\DR�\DS\DS�\DT\DT�\DU\DU��DV�DV�\DW\DW�\DX\DX�\DY\DY��DZ�DZ��D[\D[��D\\D\��D]�D]�\D^�D^�\D_\D_�\D`\D`��Da\Da�\Db�Db�\Dc\Dc��Dd\Dd�\De\De�\Df\Df��Dg\Dg��Dh�Dh�\Di\Di�\Dj\Dj��Dk�Dk�\Dl\Dl�\Dm\Dm�\Dn�Dn�\Do�Do�\Dp�Dp��Dq\Dq�\Dr\Dr�\Ds\Ds�\Dt�Dt�\Du\Du��Dv\Dv�\Dw\Dw�\Dw�Dy��D�M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aĺ^A���AĸRAĸRAĸRA���A�A�ĜA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A��
A��
A��
A��/A��;A��HA��;A��;A��;A��#A��HA��TA��HA��TA��TA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A��A��A��A��A��mA��;AĸRA�O�A��A��
Aò-A�1'A���A�C�A��A��TA���A��A��-A�VA�jA��RA�VA��mA��A��9A��DA�S�A���A���A�-A��jA��jA�-A��HA��\A�|�A�A��A��TA�t�A�1'A�  A�jA�VA��A��A�?}A���A��yA���A��jA�=qA��A���A��wA���A�&�A�
=A���A���A�E�AXA|��Az��AxJAr�!ApJAk�Ah�Ae�AaA`�DA_�mA]��AZȴAV�AR��AQp�AP�RAPA�AM��ALffAK��AJ9XAG��AD�yA�A=qA��AVA�7A��AA��A�/A��AĜA1'AK�A�/AffAƨA�A�/A�AI�A��A
  A	��A	S�AjA`BA�At�A�9An�A�7A M�@�~�@�j@�C�@���@�A�@��R@���@�1'@���@���@�@@�+@�7L@�+@�@�^@�7L@�1'@�w@��@�5?@��@�hs@�A�@�O�@�;d@�ff@���@���@�hs@�I�@�o@�=q@��@�z�@�I�@�bN@�w@��H@�X@���@އ+@�5?@�^5@ޟ�@�~�@�J@�&�@�  @�9X@��#@܃@��
@�"�@�G�@�j@��m@�C�@�v�@�Z@�z�@�r�@� �@�|�@ҏ\@��@�Q�@�%@��@�"�@��/@�Z@�5?@�hs@�V@��y@�C�@˅@��@��@ɉ7@�(�@�"�@��@���@�V@��@���@���@�@��@�?}@��
@�K�@���@���@�Q�@��9@��;@�o@���@��y@��@�C�@���@���@�(�@��P@�5?@�G�@�r�@�Z@��D@���@�A�@��@��@��@�j@�;d@��@��H@�V@��^@��@��/@��@���@�r�@��@���@�;d@���@�~�@�ff@�E�@��@��@�@�O�@�/@�%@���@���@���@��`@���@��j@��j@�Ĝ@�bN@�|�@��@��!@��\@�5?@���@��h@���@��9@��@�r�@�  @��F@�dZ@�o@��!@�~�@�ff@�{@��#@���@�X@�7L@�V@�Ĝ@��@���@��u@�bN@��@�ƨ@�dZ@�+@��R@���@�V@�$�@��T@���@�p�@�`B@��@��@��@���@�r�@�(�@��@�  @��@��@��m@���@��
@��F@�t�@�"�@�J@�p�@�x�@�hs@�X@��/@��u@�Z@�9X@�ƨ@�|�@�+@�@��\@�M�@���@�x�@�?}@��`@��/@��D@�Q�@��;@���@�|�@�
=@��!@��\@�ff@���@�&�@�Ĝ@��@���@��@�S�@�
=@��y@���@���@�=q@��T@�@��h@�`B@�7L@��j@�A�@� �@��m@�C�@��@��@��@��R@�~�@���@���@��h@�x�@�X@�V@���@��@�Q�@��@��@��w@�S�@�@���@�~�@�E�@�{@��#@���@�`B@�O�@�?}@��]@{�6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aĺ^A���AĸRAĸRAĸRA���A�A�ĜA�ƨA�ƨA���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A��
A��
A��
A��/A��;A��HA��;A��;A��;A��#A��HA��TA��HA��TA��TA��yA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A���A���A��A��A��A��A��mA��;AĸRA�O�A��A��
Aò-A�1'A���A�C�A��A��TA���A��A��-A�VA�jA��RA�VA��mA��A��9A��DA�S�A���A���A�-A��jA��jA�-A��HA��\A�|�A�A��A��TA�t�A�1'A�  A�jA�VA��A��A�?}A���A��yA���A��jA�=qA��A���A��wA���A�&�A�
=A���A���A�E�AXA|��Az��AxJAr�!ApJAk�Ah�Ae�AaA`�DA_�mA]��AZȴAV�AR��AQp�AP�RAPA�AM��ALffAK��AJ9XAG��AD�yA�A=qA��AVA�7A��AA��A�/A��AĜA1'AK�A�/AffAƨA�A�/A�AI�A��A
  A	��A	S�AjA`BA�At�A�9An�A�7A M�@�~�@�j@�C�@���@�A�@��R@���@�1'@���@���@�@@�+@�7L@�+@�@�^@�7L@�1'@�w@��@�5?@��@�hs@�A�@�O�@�;d@�ff@���@���@�hs@�I�@�o@�=q@��@�z�@�I�@�bN@�w@��H@�X@���@އ+@�5?@�^5@ޟ�@�~�@�J@�&�@�  @�9X@��#@܃@��
@�"�@�G�@�j@��m@�C�@�v�@�Z@�z�@�r�@� �@�|�@ҏ\@��@�Q�@�%@��@�"�@��/@�Z@�5?@�hs@�V@��y@�C�@˅@��@��@ɉ7@�(�@�"�@��@���@�V@��@���@���@�@��@�?}@��
@�K�@���@���@�Q�@��9@��;@�o@���@��y@��@�C�@���@���@�(�@��P@�5?@�G�@�r�@�Z@��D@���@�A�@��@��@��@�j@�;d@��@��H@�V@��^@��@��/@��@���@�r�@��@���@�;d@���@�~�@�ff@�E�@��@��@�@�O�@�/@�%@���@���@���@��`@���@��j@��j@�Ĝ@�bN@�|�@��@��!@��\@�5?@���@��h@���@��9@��@�r�@�  @��F@�dZ@�o@��!@�~�@�ff@�{@��#@���@�X@�7L@�V@�Ĝ@��@���@��u@�bN@��@�ƨ@�dZ@�+@��R@���@�V@�$�@��T@���@�p�@�`B@��@��@��@���@�r�@�(�@��@�  @��@��@��m@���@��
@��F@�t�@�"�@�J@�p�@�x�@�hs@�X@��/@��u@�Z@�9X@�ƨ@�|�@�+@�@��\@�M�@���@�x�@�?}@��`@��/@��D@�Q�@��;@���@�|�@�
=@��!@��\@�ff@���@�&�@�Ĝ@��@���@��@�S�@�
=@��y@���@���@�=q@��T@�@��h@�`B@�7L@��j@�A�@� �@��m@�C�@��@��@��@��R@�~�@���@���@��h@�x�@�X@�V@���@��@�Q�@��@��@��w@�S�@�@���@�~�@�E�@�{@��#@���@�`B@�O�@�?}@��]@{�6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bx�Bw�Bx�Bx�Bx�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bx�Bw�Bw�Bw�Bx�Bw�Bw�Bx�Bw�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�By�By�By�By�By�Bz�Bz�Bz�By�Bz�Bz�B{�Bz�Bz�B{�B{�B{�B{�B{�B}�B~�B�B�B�B�%B�=B��B��B�sB�BB�B+B%�B"�B�B�B{B%B�B�;BbB<jBbNBaHB\)B\)B[#BbNBq�Br�Bo�Bl�B`BB8RBPB��B��B�'B��B��B��B��B�\B� Bs�Bn�BaHB=qBhB
�B
�B
��B
�-B
�JB
p�B
`BB
N�B
8RB
)�B
�B
JB
B	�B	�B	��B	��B	�JB	q�B	^5B	N�B	>wB	7LB	2-B	%�B	�B	B��B�B��B	  B��B��B��B�B�sB�HB�PB�DB�7B�%B�B� B�B|�B}�Bz�B{�B�B�oB��B�B�B�B��B��B��B��B��B�B�^B��B��B��BBÖBBĜBŢBÖBÖBBB��B��BÖBĜBɺB��B�)B�B�B�)B�/B�;B�;B�BB�NB�TB�`B�B��B	�B	�B	$�B	2-B	49B	33B	6FB	8RB	7LB	7LB	49B	33B	6FB	?}B	D�B	E�B	D�B	?}B	;dB	:^B	A�B	F�B	I�B	L�B	N�B	O�B	N�B	R�B	`BB	_;B	\)B	[#B	VB	S�B	R�B	P�B	M�B	J�B	S�B	XB	XB	XB	YB	P�B	XB	^5B	bNB	[#B	Q�B	N�B	J�B	J�B	VB	_;B	ffB	jB	o�B	q�B	n�B	iyB	gmB	e`B	m�B	q�B	q�B	q�B	p�B	q�B	q�B	o�B	l�B	l�B	l�B	l�B	v�B	z�B	y�B	z�B	|�B	|�B	}�B	�B	�bB	�uB	�oB	�VB	�1B	�B	�B	�7B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�?B	�RB	�RB	�^B	�^B	�jB	�qB	�qB	�qB	�}B	B	ŢB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�5B	�BB	�HB	�NB	�TB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B

=B
DB
JB
JB
JB
JB
PB
VB
VB
VB
VB
VB
VB
\B
\B
\B
bB
bB
hB
hB
hB
oB
oB
oB
hB
hB
hB
hB
uB
�B
"222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  Bx�Bw�Bx�Bx�Bx�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bw�Bx�Bw�Bw�Bw�Bx�Bw�Bw�Bx�Bw�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�Bx�By�By�By�By�By�Bz�Bz�Bz�By�Bz�Bz�B{�Bz�Bz�B{�B{�B{�B{�B{�B}�B~�B�B�B�B�%B�=B��B��B�sB�BB�B+B%�B"�B�B�B{B%B�B�;BbB<jBbNBaHB\)B\)B[#BbNBq�Br�Bo�Bl�B`BB8RBPB��B��B�'B��B��B��B��B�\B� Bs�Bn�BaHB=qBhB
�B
�B
��B
�-B
�JB
p�B
`BB
N�B
8RB
)�B
�B
JB
B	�B	�B	��B	��B	�JB	q�B	^5B	N�B	>wB	7LB	2-B	%�B	�B	B��B�B��B	  B��B��B��B�B�sB�HB�PB�DB�7B�%B�B� B�B|�B}�Bz�B{�B�B�oB��B�B�B�B��B��B��B��B��B�B�^B��B��B��BBÖBBĜBŢBÖBÖBBB��B��BÖBĜBɺB��B�)B�B�B�)B�/B�;B�;B�BB�NB�TB�`B�B��B	�B	�B	$�B	2-B	49B	33B	6FB	8RB	7LB	7LB	49B	33B	6FB	?}B	D�B	E�B	D�B	?}B	;dB	:^B	A�B	F�B	I�B	L�B	N�B	O�B	N�B	R�B	`BB	_;B	\)B	[#B	VB	S�B	R�B	P�B	M�B	J�B	S�B	XB	XB	XB	YB	P�B	XB	^5B	bNB	[#B	Q�B	N�B	J�B	J�B	VB	_;B	ffB	jB	o�B	q�B	n�B	iyB	gmB	e`B	m�B	q�B	q�B	q�B	p�B	q�B	q�B	o�B	l�B	l�B	l�B	l�B	v�B	z�B	y�B	z�B	|�B	|�B	}�B	�B	�bB	�uB	�oB	�VB	�1B	�B	�B	�7B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�?B	�RB	�RB	�^B	�^B	�jB	�qB	�qB	�qB	�}B	B	ŢB	ǮB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�5B	�BB	�HB	�NB	�TB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B

=B
DB
JB
JB
JB
JB
PB
VB
VB
VB
VB
VB
VB
\B
\B
\B
bB
bB
hB
hB
hB
oB
oB
oB
hB
hB
hB
hB
uB
�B
"222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190553                              AO  ARCAADJP                                                                    20181005190553    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190553  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190553  QCF$                G�O�G�O�G�O�8000            