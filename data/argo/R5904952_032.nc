CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:13Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190513  20181005190513  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL                A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׷e�n1   @׷e�n�@2&�x���c�I�^51   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                       B   B   B   @   @�  @�  A   AffA>ffA`  A�  A�  A�33A�  A�  A�  A�  A�33B   B  B  B  B ffB(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C�fC  C
  C  C  C  C  C  C  C  C�C�C�C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  Cv  Cx  Cz  C|  C~  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  Cv  Cx  Cz  C|  C~  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C��C�  C��3C��3C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D	  D	�fD
  D
y�D
��D� D  D� D  D� D  Dy�D��Dy�D  D� DfD� D  D� D  D� D  D�fDfD�fDfD� D  Dy�D  D�fD  D� D��D� DfD� D  D� DfD�fDfD� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&�fD'fD'�fD(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-y�D.  D.�fD/fD/�fD0fD0� D1  D1� D2  D2y�D3  D3� D3��D4� D5  D5� D6  D6�fD7  D7� D8  D8�fD9fD9�fD:fD:� D;  D;� D<  D<y�D=  D=�fD>  D>� D?fD?� D@  D@� DA  DA�fDB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DGy�DH  DH� DH��DI� DI��DJ� DKfDK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDTfDT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_fD_�fD`fD`� D`��Day�Da��Db� DcfDc� Dc��Ddy�De  De�fDffDf� Dg  Dg� Dh  Dh� Dh��Diy�Dj  Dj� Dk  Dk� Dl  Dl� DmfDm�fDn  Dn� Do  Do�fDp  Dpy�Dp��Dq� Dr  Dr� Ds  Dsy�Ds��Dty�Du  Duy�Dv  Dv�fDw  Dw� Dw�fDy�HD�;�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @/\)@��@ǮA�
A"=pAB=pAc�
A��A��A��A��A��A��A��A��B ��B��B��B��B!\)B(��B0��B8��B@��BH��BQ\)BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B̮BЮB�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qCWC=qC#�C=qC
=qC=qC=qC=qC=qC=qC=qC=qCWCWCWC #�C"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCv=qCx=qCz=qC|=qC~=qC��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��Cv=qCx=qCz=qC|=qC~=qC��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C�+�C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��D \D �\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D�D�\D	\D	��D
\D
��D�D�\D\D�\D\D�\D\D��D�D��D\D�\D�D�\D\D�\D\D�\D\D��D�D��D�D�\D\D��D\D��D\D�\D�D�\D�D�\D\D�\D�D��D�D�\D\D�\D \D �\D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%��D&\D&��D'�D'��D(\D(�\D)\D)��D*\D*�\D+\D+�\D,\D,�\D-\D-��D.\D.��D/�D/��D0�D0�\D1\D1�\D2\D2��D3\D3�\D4�D4�\D5\D5�\D6\D6��D7\D7�\D8\D8��D9�D9��D:�D:�\D;\D;�\D<\D<��D=\D=��D>\D>�\D?�D?�\D@\D@�\DA\DA��DB\DB�\DC\DC�\DD\DD�\DE\DE�\DF\DF�\DG\DG��DH\DH�\DI�DI�\DJ�DJ�\DK�DK�\DL\DL�\DM\DM�\DN\DN��DO\DO�\DP\DP�\DQ\DQ�\DR\DR�\DS\DS��DT�DT�\DU\DU�\DV\DV�\DW\DW�\DX\DX�\DY\DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^��D_�D_��D`�D`�\Da�Da��Db�Db�\Dc�Dc�\Dd�Dd��De\De��Df�Df�\Dg\Dg�\Dh\Dh�\Di�Di��Dj\Dj�\Dk\Dk�\Dl\Dl�\Dm�Dm��Dn\Dn�\Do\Do��Dp\Dp��Dq�Dq�\Dr\Dr�\Ds\Ds��Dt�Dt��Du\Du��Dv\Dv��Dw\Dw�\Dw��Dy��D�C�D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A���A���A���A�1A�
=A�VA�VA�oA�VA�VA�VA�oA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�"�A�"�A�"�A� �A�1AӺ^AғuAН�A���A�I�A���A͡�A͍PA�p�A�33A���A̍PA��A�G�A��A��A�$�A���A�p�Aȕ�A�A�%Aƕ�A�(�A�+A�"�AŁA�n�A�ȴA��A��A�^5A��A�p�A��^A��wA�t�A���A�;dA���A�A���A�p�A��9A�1'A��7A��
A�?}A���A��!A�M�A��-A���A���A��wA��A�$�A��mA�=qAehsAc��Aa��A`v�A_\)A]�TAY�AU;dASx�AR�+AO�wAM�ALjAJ��AJ$�AIXAH�9AG
=AEG�AD1'AC��AC/AehsAc��Aa��A`v�A_\)A]�TAY�AU;dASx�AR�+AO�wAM�ALjAJ��AJ$�AIXAH�9AG
=AEG�AD1'AC��AC/AB1AA��A@��A<��A:�+A9l�A9�A7�A6��A5��A4�!A3"�A2�`A2$�A1��A/&�A+�A)33A(�jA(�`A(��A(^5A'S�A&�A&�yA$�A#A#hsA#��A#��A#p�A!�TA (�AĜAAĜA��A?}A�PAO�A�9A��A\)A�9Av�At�A�A��A�`A��A �A
�/A	K�AQ�A7LA�9A-A��A��A~�A��Ap�AG�A ��A �+@��@��R@�Ĝ@�
=@�ȴ@�M�@�/@�@���@�+@��@��@��m@�M�@��@��@���@�V@�G�@���@���@�j@�@�v�@�/@�1'@�w@�  @�!@�ff@��
@�F@�M�@�@�@�^5@��@�!@�%@���@�9X@��y@�!@�ff@�E�@���@�X@� �@��y@ݩ�@�1@�C�@�ƨ@ڏ\@��T@�Ĝ@ׅ@Ԭ@���@���@�Ĝ@�S�@�o@ΰ!@�C�@ϕ�@�33@�@�33@�\)@�;d@�@͡�@��@͑h@̛�@ʏ\@Ɨ�@���@�p�@���@ÍP@�1'@¸R@�J@��y@��H@��H@�M�@�Z@�Z@���@���@�A�@��
@�dZ@�+@�|�@��@�@�$�@��@���@��P@���@��`@���@���@��@�bN@�9X@��F@�33@��y@��@��-@��7@�G�@�/@�x�@��@��u@��;@�
=@��@�l�@���@��7@�&�@���@��j@��@�A�@�b@��@���@���@��H@�o@�K�@�  @�I�@�S�@�v�@��@�+@���@�ff@�/@�ƨ@��@�C�@���@��\@�n�@�$�@��^@���@�X@��@��9@��@�bN@�I�@� �@��@�ƨ@�ƨ@�ƨ@�ƨ@��w@��w@���@���@�\)@�@�ȴ@�v�@�x�@�X@��@���@��`@��j@��@� �@���@��@�dZ@�+@��y@���@�V@�J@���@�p�@�G�@�&�@��/@�bN@�b@��;@���@�K�@��!@�E�@�J@���@���@���@�?}@��@��j@��@�1'@�9X@�1@��P@�"�@��H@���@��R@�ff@�-@��@��^@�x�@��@���@��j@��@���@�z�@�  @�t�@�33@�"�@��@��@��y@���@���@��^@�%@�Ĝ@�r�@��w@�l�@�K�@�C�@�;d@�;d@�33@�"�@�+@�K�@�C�@�
=@��@��R@��\@�=q@�@���@���@��@�X@��@�G�@�J@��h@��D@�Q�@��P@�K�@�C�@�
=@���@��!@��\@�M�@���@���@���@��j@���@�j@���@� i@o�6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A���A���A���A���A���A���A���A�1A�
=A�VA�VA�oA�VA�VA�VA�oA��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�"�A�"�A�"�A� �A�1AӺ^AғuAН�A���A�I�A���A͡�A͍PA�p�A�33A���A̍PA��A�G�A��A��A�$�A���A�p�Aȕ�A�A�%Aƕ�A�(�A�+A�"�AŁA�n�A�ȴA��A��A�^5A��A�p�A��^A��wA�t�A���A�;dA���A�A���A�p�A��9A�1'A��7A��
A�?}A���A��!A�M�A��-A���A���A��wA��A�$�A��mA�=qAehsAc��Aa��A`v�A_\)A]�TAY�AU;dASx�AR�+AO�wAM�ALjAJ��AJ$�AIXAH�9AG
=AEG�AD1'AC��AC/AehsAc��Aa��A`v�A_\)A]�TAY�AU;dASx�AR�+AO�wAM�ALjAJ��AJ$�AIXAH�9AG
=AEG�AD1'AC��AC/AB1AA��A@��A<��A:�+A9l�A9�A7�A6��A5��A4�!A3"�A2�`A2$�A1��A/&�A+�A)33A(�jA(�`A(��A(^5A'S�A&�A&�yA$�A#A#hsA#��A#��A#p�A!�TA (�AĜAAĜA��A?}A�PAO�A�9A��A\)A�9Av�At�A�A��A�`A��A �A
�/A	K�AQ�A7LA�9A-A��A��A~�A��Ap�AG�A ��A �+@��@��R@�Ĝ@�
=@�ȴ@�M�@�/@�@���@�+@��@��@��m@�M�@��@��@���@�V@�G�@���@���@�j@�@�v�@�/@�1'@�w@�  @�!@�ff@��
@�F@�M�@�@�@�^5@��@�!@�%@���@�9X@��y@�!@�ff@�E�@���@�X@� �@��y@ݩ�@�1@�C�@�ƨ@ڏ\@��T@�Ĝ@ׅ@Ԭ@���@���@�Ĝ@�S�@�o@ΰ!@�C�@ϕ�@�33@�@�33@�\)@�;d@�@͡�@��@͑h@̛�@ʏ\@Ɨ�@���@�p�@���@ÍP@�1'@¸R@�J@��y@��H@��H@�M�@�Z@�Z@���@���@�A�@��
@�dZ@�+@�|�@��@�@�$�@��@���@��P@���@��`@���@���@��@�bN@�9X@��F@�33@��y@��@��-@��7@�G�@�/@�x�@��@��u@��;@�
=@��@�l�@���@��7@�&�@���@��j@��@�A�@�b@��@���@���@��H@�o@�K�@�  @�I�@�S�@�v�@��@�+@���@�ff@�/@�ƨ@��@�C�@���@��\@�n�@�$�@��^@���@�X@��@��9@��@�bN@�I�@� �@��@�ƨ@�ƨ@�ƨ@�ƨ@��w@��w@���@���@�\)@�@�ȴ@�v�@�x�@�X@��@���@��`@��j@��@� �@���@��@�dZ@�+@��y@���@�V@�J@���@�p�@�G�@�&�@��/@�bN@�b@��;@���@�K�@��!@�E�@�J@���@���@���@�?}@��@��j@��@�1'@�9X@�1@��P@�"�@��H@���@��R@�ff@�-@��@��^@�x�@��@���@��j@��@���@�z�@�  @�t�@�33@�"�@��@��@��y@���@���@��^@�%@�Ĝ@�r�@��w@�l�@�K�@�C�@�;d@�;d@�33@�"�@�+@�K�@�C�@�
=@��@��R@��\@�=q@�@���@���@��@�X@��@�G�@�J@��h@��D@�Q�@��P@�K�@�C�@�
=@���@��!@��\@�M�@���@���@���@��j@���@�j@���@� i@o�6111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
� B
� B
� B
� B
� B
�B
�%B
|�B
l�B
n�B
�JB
��B
��B
�B
�'B
�RB
�qB
ĜB
��B
�B
�#B
�/B
�B
��BB�B2-BI�BaHB�VB��B�B�dB�;BVB/B^5Bs�B�VB�BhsB<jB�B�B�B'�B$�B+B �B��B�HB��BɺBȴB��B��BɺBÖB�B��BgmBB�B�B
�B|�B	s�B	iyB	]/B	T�B	M�B	C�B	1'B	�B	uB	VB	B��B��B�B�B�B�fB�BB�#B�
B��B��B	s�B	iyB	]/B	T�B	M�B	C�B	1'B	�B	uB	VB	B��B��B�B�B�B�fB�BB�#B�
B��B��B��B��B��BĜB��B�jB�dB�XB�XB�LB�FB�3B�3B�FB�qB�}B�!B��B��B��B�B��B��B�9B�?B�B��B�B�9B�FB��BÖBĜB��B�wB�XB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�B�3B�FB�LB�wBɺBƨB��B�dB�LB�FBÖB��B��B��B��B�5B�B		7B		7B��B��B	JB	0!B	6FB	7LB	<jB	N�B	S�B	YB	ZB	YB	YB	YB	\)B	[#B	]/B	_;B	^5B	cTB	cTB	dZB	aHB	gmB	l�B	iyB	ffB	cTB	e`B	m�B	l�B	k�B	iyB	dZB	[#B	T�B	P�B	L�B	H�B	N�B	Q�B	YB	\)B	\)B	\)B	^5B	_;B	aHB	dZB	ffB	iyB	iyB	ffB	aHB	W
B	W
B	\)B	aHB	_;B	e`B	`BB	_;B	dZB	ffB	XB	[#B	l�B	n�B	m�B	iyB	dZB	e`B	iyB	l�B	p�B	z�B	�%B	�%B	�7B	�+B	�B	�B	�DB	�VB	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�LB	�qB	��B	�}B	�qB	��B	ÖB	��B	��B	�qB	�dB	�dB	�dB	�qB	�}B	��B	��B	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�HB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
DB
DB
1B
1B
%B
%B
+B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
PB
PB
VB
�B
%`B
0�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
~�B
~�B
~�B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
� B
� B
� B
� B
� B
�B
�%B
|�B
l�B
n�B
�JB
��B
��B
�B
�'B
�RB
�qB
ĜB
��B
�B
�#B
�/B
�B
��BB�B2-BI�BaHB�VB��B�B�dB�;BVB/B^5Bs�B�VB�BhsB<jB�B�B�B'�B$�B+B �B��B�HB��BɺBȴB��B��BɺBÖB�B��BgmBB�B�B
�B|�B	s�B	iyB	]/B	T�B	M�B	C�B	1'B	�B	uB	VB	B��B��B�B�B�B�fB�BB�#B�
B��B��B	s�B	iyB	]/B	T�B	M�B	C�B	1'B	�B	uB	VB	B��B��B�B�B�B�fB�BB�#B�
B��B��B��B��B��BĜB��B�jB�dB�XB�XB�LB�FB�3B�3B�FB�qB�}B�!B��B��B��B�B��B��B�9B�?B�B��B�B�9B�FB��BÖBĜB��B�wB�XB�FB�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B�B�3B�FB�LB�wBɺBƨB��B�dB�LB�FBÖB��B��B��B��B�5B�B		7B		7B��B��B	JB	0!B	6FB	7LB	<jB	N�B	S�B	YB	ZB	YB	YB	YB	\)B	[#B	]/B	_;B	^5B	cTB	cTB	dZB	aHB	gmB	l�B	iyB	ffB	cTB	e`B	m�B	l�B	k�B	iyB	dZB	[#B	T�B	P�B	L�B	H�B	N�B	Q�B	YB	\)B	\)B	\)B	^5B	_;B	aHB	dZB	ffB	iyB	iyB	ffB	aHB	W
B	W
B	\)B	aHB	_;B	e`B	`BB	_;B	dZB	ffB	XB	[#B	l�B	n�B	m�B	iyB	dZB	e`B	iyB	l�B	p�B	z�B	�%B	�%B	�7B	�+B	�B	�B	�DB	�VB	�\B	�bB	�hB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�LB	�qB	��B	�}B	�qB	��B	ÖB	��B	��B	�qB	�dB	�dB	�dB	�qB	�}B	��B	��B	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�;B	�BB	�HB	�BB	�BB	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�TB	�ZB	�ZB	�`B	�`B	�ZB	�ZB	�ZB	�ZB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
  B
  B
  B
  B
B
B
B
B
B
B
B
%B
DB
DB
1B
1B
%B
%B
+B
	7B
	7B
	7B
	7B

=B

=B
DB
JB
PB
PB
VB
�B
%`B
0�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222224242222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190513                              AO  ARCAADJP                                                                    20181005190513    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190513  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190513  QCF$                G�O�G�O�G�O�C000            