CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:58Z creation      
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
resolution        =���   axis      Z        \  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J    PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     \  K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  S4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  \h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  c�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  l�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  tT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  v,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  }�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     \  `   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190558  20181005190558  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i�߉1   @��j'Ғ@0�Q���c�G�z�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @@  @�  @�33A   A   AA��Aa��A�  A�  A�  A�33A�  A���A�  A�  B   B  B  B  B   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.�C/�fC2  C4  C6  C7�fC:  C<  C>  C@  CB  CD�CF  CH  CI�fCL  CN  CP  CQ�fCS�fCV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp�Cr�Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  D   D y�D  D� D  D� DfD�fD  D� DfD�fD  Dy�D  Dy�D��D� D	  D	� D
  D
� D  D� DfD� D  D� D  D� D  D� DfD�fD  D� D  D� D  Dy�D��D� D  Dy�D��Dy�D��D� DfD�fDfD� D  D� D  D� D  D� D  Dy�D��D� D  D�fD fD �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0fD0� D0��D1� D2  D2y�D3  D3�fD4  D4� D4��D5� D6  D6�fD7fD7�fD8fD8�fD9  D9y�D9��D:� D;  D;�fD<fD<� D=fD=� D=��D>y�D?  D?� D?��D@� DA  DAy�DB  DB� DC  DCy�DC��DDy�DD��DEy�DE��DFy�DF��DG� DH  DH�fDIfDI� DI��DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO�fDPfDP�fDQ  DQ� DR  DR� DSfDS�fDT  DT�fDT��DUy�DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^�fD_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Df��Dgy�Dh  Dh� Dh��Diy�Di��Djy�Dy{�D�5�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @O\)@��@��GA�
A#�
AEp�Aep�A��A��A��A��A��AҸRA��A��B ��B��B��B��B ��B(��B0��B9\)B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��B�z�B�z�C =qC=qC=qC=qC=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,WC.WC0#�C2=qC4=qC6=qC8#�C:=qC<=qC>=qC@=qCB=qCDWCF=qCH=qCJ#�CL=qCN=qCP=qCR#�CT#�CV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd#�Cf=qCh=qCj=qCl=qCn=qCpWCrWCt=qCv=qCx=qCz=qC|=qC~=qC�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C�+�C��C�+�C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D \D ��D\D�\D\D�\D�D��D\D�\D�D��D\D��D\D��D�D�\D	\D	�\D
\D
�\D\D�\D�D�\D\D�\D\D�\D\D�\D�D��D\D�\D\D�\D\D��D�D�\D\D��D�D��D�D�\D�D��D�D�\D\D�\D\D�\D\D�\D\D��D�D�\D\D��D �D ��D!\D!�\D"\D"�\D#\D#�\D$\D$�\D%\D%�\D&\D&�\D'\D'�\D(\D(�\D)\D)�\D*�D*�\D+\D+�\D,\D,�\D-\D-�\D.\D.�\D/\D/�\D0�D0�\D1�D1�\D2\D2��D3\D3��D4\D4�\D5�D5�\D6\D6��D7�D7��D8�D8��D9\D9��D:�D:�\D;\D;��D<�D<�\D=�D=�\D>�D>��D?\D?�\D@�D@�\DA\DA��DB\DB�\DC\DC��DD�DD��DE�DE��DF�DF��DG�DG�\DH\DH��DI�DI�\DJ�DJ�\DK\DK��DL\DL�\DM\DM�\DN\DN�\DO\DO��DP�DP��DQ\DQ�\DR\DR�\DS�DS��DT\DT��DU�DU��DV\DV�\DW\DW�\DX\DX�\DY�DY�\DZ\DZ�\D[\D[�\D\\D\�\D]\D]�\D^\D^��D_\D_�\D`�D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd�Dd�\De\De�\Df\Df�\Dg�Dg��Dh\Dh�\Di�Di��Dj�Dj��Dy��D�=q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�A�S�A�Q�A�Q�A�O�A�M�A�M�A�O�A�S�A�XA�^5A�^5A�^5A�^5A�`BA�^5A�^5A�`BA�dZA�dZA�ffA�ffA�ffA�dZA�ffA�hsA�hsA�jA�jA�jA�jA�l�A�l�A�p�A�t�A��A��mA�$�A��A�1A���A���A��mA���A�r�A�C�A���A�5?A�%AƲ-A�A���AŶFA�n�A��AĬA�33A��#A�t�A�+A�t�A�33A�dZA���A���A�A�/A�A�A��A�l�A��A��;A��yA���A��RA�"�A�n�A�^5A���A���A�ffA��A���A�ffA���A�
=A��RA�~�A��A��A��A�-A�;dA��-A�7LA��A�r�A�n�A�1A�A��hA�x�A��/A��DA��A{G�Ax�Au7LAq��Am�AlbAjVAd�AbM�A_7LAYl�AW��AW��AV5?AS|�AQ�7AN�!AL�DAJZAH��AG�AFjAD1AA?}A?�FA<��A;t�A:�DA9�mA8�A7\)A65?A4~�A2��A1��A0�A.�A,A�A+�A*ZA)�PA(�`A(��A(1'A%�-A$��A$ffA#�A#"�A"�RA"n�A"5?A!�A!S�A n�A��A|�AQ�An�Al�A��AffAJA��A&�A\)A��A�A{A�7AS�A �A	�A`BAAz�A�\A�AA��A`BA�AK�AXAo@�K�@�~�@��@�bN@��h@��@��@��@�!@�&�@�  @�@��@�7L@�@땁@�@�o@��H@柾@�Ĝ@㕁@�+@�!@�-@�?}@���@��@�V@�X@���@�r�@�Q�@�ƨ@ڏ\@�-@���@�33@�=q@Ձ@��m@��@��@�?}@�Ĝ@Ϯ@��y@�ȴ@�@́@�G�@�  @�~�@�-@�J@��T@���@�^5@��@�=q@��T@�G�@ț�@��
@�|�@�K�@�@�
=@�^5@�?}@��@ă@Õ�@\@�p�@���@���@���@���@�@���@���@��@�V@�ƨ@�|�@���@�$�@��h@�J@�ȴ@�~�@�5?@�p�@��@���@�G�@�j@�b@���@��w@��y@���@��+@�5?@��^@��7@�%@��9@��D@�A�@���@�K�@��@��\@��^@�/@��9@�j@��u@�I�@���@��@�@��@��T@��@���@��h@�x�@�7L@�&�@�/@�7L@�&�@�V@���@�j@�A�@�1'@�b@���@��F@��P@�dZ@�K�@�+@�
=@�33@��@��H@���@�ff@�-@���@�&�@���@���@��D@�bN@��
@�ȴ@�E�@���@��@���@��-@���@��@��u@��m@��@�l�@�o@���@�V@�E�@�^5@�ff@�n�@�dZ@�S�@�C�@���@��@��;@��;@���@�1@��
@��P@��@�t�@��@���@��h@�7L@�Ĝ@�r�@���@��u@�b@��@�ȴ@��!@��\@�~�@�ff@���@��H@���@��@��T@��h@�O�@�?}@��@��@�A�@���@��
@���@�@���@�v�@�-@�5?@�-@�$�@�-@���@�`B@���@��m@�;d@�
=@���@�ff@�$�@��-@���@�hs@�V@��@�%@���@�I�@��@�b@�(�@��@��@�bN@�1'@�ƨ@�"�@���@�E�@�$�@���@�hs@�%@���@��@�r�@�I�@�I�@�I�@�b@��@�|�@���@��R@��R@��!@���@���@���@��\@��R@��v111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�S�A�S�A�Q�A�Q�A�O�A�M�A�M�A�O�A�S�A�XA�^5A�^5A�^5A�^5A�`BA�^5A�^5A�`BA�dZA�dZA�ffA�ffA�ffA�dZA�ffA�hsA�hsA�jA�jA�jA�jA�l�A�l�A�p�A�t�A��A��mA�$�A��A�1A���A���A��mA���A�r�A�C�A���A�5?A�%AƲ-A�A���AŶFA�n�A��AĬA�33A��#A�t�A�+A�t�A�33A�dZA���A���A�A�/A�A�A��A�l�A��A��;A��yA���A��RA�"�A�n�A�^5A���A���A�ffA��A���A�ffA���A�
=A��RA�~�A��A��A��A�-A�;dA��-A�7LA��A�r�A�n�A�1A�A��hA�x�A��/A��DA��A{G�Ax�Au7LAq��Am�AlbAjVAd�AbM�A_7LAYl�AW��AW��AV5?AS|�AQ�7AN�!AL�DAJZAH��AG�AFjAD1AA?}A?�FA<��A;t�A:�DA9�mA8�A7\)A65?A4~�A2��A1��A0�A.�A,A�A+�A*ZA)�PA(�`A(��A(1'A%�-A$��A$ffA#�A#"�A"�RA"n�A"5?A!�A!S�A n�A��A|�AQ�An�Al�A��AffAJA��A&�A\)A��A�A{A�7AS�A �A	�A`BAAz�A�\A�AA��A`BA�AK�AXAo@�K�@�~�@��@�bN@��h@��@��@��@�!@�&�@�  @�@��@�7L@�@땁@�@�o@��H@柾@�Ĝ@㕁@�+@�!@�-@�?}@���@��@�V@�X@���@�r�@�Q�@�ƨ@ڏ\@�-@���@�33@�=q@Ձ@��m@��@��@�?}@�Ĝ@Ϯ@��y@�ȴ@�@́@�G�@�  @�~�@�-@�J@��T@���@�^5@��@�=q@��T@�G�@ț�@��
@�|�@�K�@�@�
=@�^5@�?}@��@ă@Õ�@\@�p�@���@���@���@���@�@���@���@��@�V@�ƨ@�|�@���@�$�@��h@�J@�ȴ@�~�@�5?@�p�@��@���@�G�@�j@�b@���@��w@��y@���@��+@�5?@��^@��7@�%@��9@��D@�A�@���@�K�@��@��\@��^@�/@��9@�j@��u@�I�@���@��@�@��@��T@��@���@��h@�x�@�7L@�&�@�/@�7L@�&�@�V@���@�j@�A�@�1'@�b@���@��F@��P@�dZ@�K�@�+@�
=@�33@��@��H@���@�ff@�-@���@�&�@���@���@��D@�bN@��
@�ȴ@�E�@���@��@���@��-@���@��@��u@��m@��@�l�@�o@���@�V@�E�@�^5@�ff@�n�@�dZ@�S�@�C�@���@��@��;@��;@���@�1@��
@��P@��@�t�@��@���@��h@�7L@�Ĝ@�r�@���@��u@�b@��@�ȴ@��!@��\@�~�@�ff@���@��H@���@��@��T@��h@�O�@�?}@��@��@�A�@���@��
@���@�@���@�v�@�-@�5?@�-@�$�@�-@���@�`B@���@��m@�;d@�
=@���@�ff@�$�@��-@���@�hs@�V@��@�%@���@�I�@��@�b@�(�@��@��@�bN@�1'@�ƨ@�"�@���@�E�@�$�@���@�hs@�%@���@��@�r�@�I�@�I�@�I�@�b@��@�|�@���@��R@��R@��!@���@���@���@��\@��R@��v111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�/B�5B�/B�;B�;B�ZB�B	bB	|�B	�NB
�B
K�B
bNB
o�B
z�B
�+B
��B
��B
��B
�BB�B2-B0!BN�BbNBp�B|�B~�B�B��B��B��B�fB��B
=B�B+B6FB?}BH�BL�BK�BI�BF�BA�B=qB=qB7LB0!B%�B�B	7B��B�B�HB�-B��B��B�7B\)B>wB%�B
��B
�fB
�B
�qB
��B
�hB
v�B
\)B
S�B
M�B
F�B
'�B
B	�B	�B	�LB	��B	�PB	t�B	^5B	S�B	F�B	,B	�B	JB�B�sB�`B�/B��BƨB�dB�9B�B�B�!B�B��B��B��B��B�{B�uB�hB�hB�hB�{B��B�uB�bB�hB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�'B�!B�dB�}B��BǮB��B��BɺBɺB��B��B��B��B��B�
B�fB�fB�B��B��B��B��B�B�mB�ZB�BB�#B�B�B�
B��BĜBȴBƨB�wB�dB�dB�qB�wB��BBBŢBƨB��B��B�B�B��B��B�
B�#B�5B�BB�BB�TB�mB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	1B	
=B	VB	bB	bB	bB	�B	�B	 �B	&�B	)�B	0!B	6FB	9XB	8RB	:^B	>wB	?}B	>wB	>wB	>wB	@�B	C�B	F�B	F�B	H�B	K�B	K�B	P�B	O�B	P�B	R�B	VB	YB	ZB	ZB	[#B	[#B	ZB	[#B	[#B	[#B	^5B	hsB	r�B	u�B	z�B	x�B	z�B	�B	�+B	�=B	�DB	�JB	�PB	�DB	�DB	�DB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�FB	�FB	�LB	�FB	�FB	�LB	�RB	�XB	�dB	�dB	�wB	�}B	�}B	��B	ĜB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�;B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B	��B	��B
  B
  B
B
B
B
B
B
B
  B
  B
  B
  B
  B
  B	��B
  B
B
B
B
B
B
B
B
%B
+B
1B
DB
DB
JB
JB
JB
PB
JB
JB
PB
VB
VB
VB
VB
VB
\B
bB
bB
bB
hB
hB
bB
bB
hB
oB
oB
oB
{B
�B
�B
B
.�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�)B�/B�5B�/B�;B�;B�ZB�B	bB	|�B	�NB
�B
K�B
bNB
o�B
z�B
�+B
��B
��B
��B
�BB�B2-B0!BN�BbNBp�B|�B~�B�B��B��B��B�fB��B
=B�B+B6FB?}BH�BL�BK�BI�BF�BA�B=qB=qB7LB0!B%�B�B	7B��B�B�HB�-B��B��B�7B\)B>wB%�B
��B
�fB
�B
�qB
��B
�hB
v�B
\)B
S�B
M�B
F�B
'�B
B	�B	�B	�LB	��B	�PB	t�B	^5B	S�B	F�B	,B	�B	JB�B�sB�`B�/B��BƨB�dB�9B�B�B�!B�B��B��B��B��B�{B�uB�hB�hB�hB�{B��B�uB�bB�hB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�'B�!B�dB�}B��BǮB��B��BɺBɺB��B��B��B��B��B�
B�fB�fB�B��B��B��B��B�B�mB�ZB�BB�#B�B�B�
B��BĜBȴBƨB�wB�dB�dB�qB�wB��BBBŢBƨB��B��B�B�B��B��B�
B�#B�5B�BB�BB�TB�mB�yB�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	B	1B	
=B	VB	bB	bB	bB	�B	�B	 �B	&�B	)�B	0!B	6FB	9XB	8RB	:^B	>wB	?}B	>wB	>wB	>wB	@�B	C�B	F�B	F�B	H�B	K�B	K�B	P�B	O�B	P�B	R�B	VB	YB	ZB	ZB	[#B	[#B	ZB	[#B	[#B	[#B	^5B	hsB	r�B	u�B	z�B	x�B	z�B	�B	�+B	�=B	�DB	�JB	�PB	�DB	�DB	�DB	�\B	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�3B	�FB	�FB	�LB	�FB	�FB	�LB	�RB	�XB	�dB	�dB	�wB	�}B	�}B	��B	ĜB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�
B	�B	�B	�B	�B	�#B	�/B	�/B	�;B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B	��B	��B
  B
  B
B
B
B
B
B
B
  B
  B
  B
  B
  B
  B	��B
  B
B
B
B
B
B
B
B
%B
+B
1B
DB
DB
JB
JB
JB
PB
JB
JB
PB
VB
VB
VB
VB
VB
\B
bB
bB
bB
hB
hB
bB
bB
hB
oB
oB
oB
{B
�B
�B
B
.�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190558                              AO  ARCAADJP                                                                    20181005190558    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190558  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190558  QCF$                G�O�G�O�G�O�8000            