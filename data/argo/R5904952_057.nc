CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:18Z creation      
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  o�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �H   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �<   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �<   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �<   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �<   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �h   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �p   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �t   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �x   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181005190518  20181005190518  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               9A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׽��چ71   @׽�W:۬@1J��n��c���"��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      9A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A���A���B   B��B��B  B   B(  B0  B7��B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  B�  B�  B�  B�  C   C  C�fC�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C��C�  C�  C�  C��C�  C��3C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� DfD�fD  D� D  D� D  D�fD  D� D	  D	� D
  D
� D  D� D��D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D�fDfD�fDfD�fDfD� D��D� D  D� D  Dy�D  D� D��D� D  D� D  D� D   D � D ��D!y�D"  D"y�D#  D#�fD$  D$y�D$��D%� D&  D&� D&��D'� D(  D(y�D)  D)� D*  D*� D+fD+�fD,  D,� D-  D-y�D.  D.� D/  D/� D0  D0y�D1  D1� D2  D2� D3  D3y�D3��D4y�D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DBy�DC  DC� DD  DD� DE  DE�fDF  DF� DG  DG� DH  DH�fDI  DIy�DI��DJ� DKfDK�fDL  DLy�DL��DM� DNfDN�fDOfDO�fDP  DPy�DP��DQy�DQ��DR� DS  DS� DT  DT� DU  DU� DVfDV�fDW  DWy�DX  DX� DY  DY� DZ  DZy�DZ��D[� D\  D\� D]fD]� D]��D^y�D_  D_� D_��D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDe  De� De��Df� Dg  Dg� Dh  Dh� Dh��Diy�Dj  Dj� Dk  Dk�fDlfDl�fDmfDm�fDn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dq��Dry�Dr��Dsy�Dt  Dt� Du  Du�fDvfDv� Dv��Dwy�Dw� Dy��D�E11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@ǮA�
A#�
AC�
Ac�
A��A��A��A��A��AҸRA�RA�RB ��B�]B�]B��B ��B(��B0��B8�]B@��BH��BQ\)BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��B��B�z�B�z�B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�BخBܮB�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC#�C#�C=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>=qC@=qCB=qCD=qCF=qCH=qCJ=qCL=qCN=qCPWCR=qCT=qCV=qCX=qCZ=qC\=qC^=qC`=qCb=qCd=qCf=qCh=qCjWCl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C�+�C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C�+�C��C��C��C�+�C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C��C��C��C��C��C��D �D �\D\D�\D\D�\D\D�\D�D��D\D�\D\D�\D\D��D\D�\D	\D	�\D
\D
�\D\D�\D�D�\D\D�\D\D�\D\D�\D�D�\D\D�\D\D�\D\D�\D\D�\D\D��D�D��D�D��D�D�\D�D�\D\D�\D\D��D\D�\D�D�\D\D�\D\D�\D \D �\D!�D!��D"\D"��D#\D#��D$\D$��D%�D%�\D&\D&�\D'�D'�\D(\D(��D)\D)�\D*\D*�\D+�D+��D,\D,�\D-\D-��D.\D.�\D/\D/�\D0\D0��D1\D1�\D2\D2�\D3\D3��D4�D4��D5\D5�\D6\D6�\D7\D7�\D8\D8��D9\D9�\D:\D:�\D;\D;�\D<\D<�\D=\D=��D>\D>�\D?\D?�\D@\D@�\DA\DA�\DB\DB��DC\DC�\DD\DD�\DE\DE��DF\DF�\DG\DG�\DH\DH��DI\DI��DJ�DJ�\DK�DK��DL\DL��DM�DM�\DN�DN��DO�DO��DP\DP��DQ�DQ��DR�DR�\DS\DS�\DT\DT�\DU\DU�\DV�DV��DW\DW��DX\DX�\DY\DY�\DZ\DZ��D[�D[�\D\\D\�\D]�D]�\D^�D^��D_\D_�\D`�D`�\Da\Da�\Db\Db�\Dc\Dc�\Dd\Dd��De\De�\Df�Df�\Dg\Dg�\Dh\Dh�\Di�Di��Dj\Dj�\Dk\Dk��Dl�Dl��Dm�Dm��Dn\Dn�\Do\Do�\Dp\Dp��Dq\Dq�\Dr�Dr��Ds�Ds��Dt\Dt�\Du\Du��Dv�Dv�\Dw�Dw��Dw�\Dy�D�L�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aٛ�A٣�Aٲ-AٶFAټjA�AٸRAٴ9AپwA�ĜA���A���A���A���A���A���A���A��HA��`AپwA�~�A�I�A؁A�K�A�+A�{A׬A�S�A��A֥�A�E�A՟�A�7LA԰!A���A�oAҡ�A���A�`BA��mA�^5A�"�Aϕ�AάA�"�A͛�A�I�A��A���A�Aˡ�A�n�A�ƨAɴ9AǏ\AƓuAēuA�|�A�$�A��;A���A��/A�oA�l�A��PA���A���A��mA���A��A���A��A��A�l�A�7LA��A�dZA��wA�ĜA��!A��A�r�A�;dA��A���A�C�A��DA��A�n�A�=qA�K�A��A�7LA��jA���A�ffA��A���A�K�A�-A���A��A�O�A�-A�{A�|�A�1'A�O�A��A|�jAx1'At�Ap�AmG�Aj�9AgK�Ad  A`�A]��A\=qAZ�9AV�AS��AR{AQdZAO|�AL �AIl�AF�AC��AC�wAB^5A@��A>VA;p�A:�A89XA7\)A7"�A6�jA41A/33A.�A-��A,��A+�7A*v�A(�A({A&r�A$�A#�TA"��A"ĜA"1'A ^5A�HAVA�DA��A��A�A^5A�wA\)A\)A�A?}At�AS�A"�AoA33AC�AoAVAƨA/A�AXAoA�`A�mAC�A��Ax�AȴAQ�A�FA
�A
�HA
~�A
ZA
��A�A�HAJA	�mA	l�A	;dA��AX@��7@�9X@��@���@�1'@�ff@�1'@�ȴ@�X@���@� �@�1@���@��@���@��@�E�@�O�@�~�@�33@���@�|�@�;d@���@�5?@�O�@�/@��@���@�O�@�|�@�@�F@�j@�A�@�R@���@��m@���@��T@�hs@���@�b@�(�@�1'@�z�@�j@�Z@�  @���@���@߾w@�t�@���@�@݉7@�bN@�o@�V@�@ى7@�V@�%@�Ĝ@�j@�I�@׶F@���@�ff@�=q@���@���@��@ёh@�1'@��
@��m@Ϯ@���@��@�%@�I�@��@���@�z�@��m@ǥ�@���@�n�@�5?@�p�@�(�@�5?@��^@���@�33@�
=@�A�@�l�@���@�&�@��9@�A�@�K�@�=q@���@�hs@��@��D@�bN@�A�@���@�|�@�dZ@�
=@�v�@���@��7@�7L@���@��D@�z�@�bN@��@��\@�M�@��T@�`B@�33@�v�@��@���@�&�@���@���@��`@�&�@�7L@��@���@��`@�Ĝ@��j@���@�bN@� �@�(�@� �@���@�t�@�\)@�@���@�p�@� �@�(�@�ƨ@�+@�"�@�"�@�33@���@��@���@�5?@���@�hs@�&�@��9@�1'@��@��
@���@��P@�t�@�K�@���@�=q@���@���@�X@�/@���@�9X@���@�t�@�+@�o@�@���@�~�@�M�@�=q@�$�@�J@��@�ff@�ff@�$�@�V@���@�|�@��H@���@�7L@���@���@��@���@���@��@�7L@��j@�A�@�+@��\@�^5@�5?@���@���@���@��@�Q�@��@��m@��m@��;@���@�+@��!@�^5@�X@��@�V@��j@�(�@�1'@�A�@�ƨ@�33@��y@�V@�J@���@��T@���@�x�@�7L@��@�%@��`@��/@��9@�(�@��@��@�bN@��@�bN@��@��;@�ƨ@��@�S�@�33@�"�@�o@���@��@��R@��R@��!@��\@�J@��^@��^@���@��7@�&�@��j@���@�r�@���@��;@�ƨ@��w@���@�\)@���@�ȴ@���@��R@��+@�p�@�[�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aٛ�A٣�Aٲ-AٶFAټjA�AٸRAٴ9AپwA�ĜA���A���A���A���A���A���A���A��HA��`AپwA�~�A�I�A؁A�K�A�+A�{A׬A�S�A��A֥�A�E�A՟�A�7LA԰!A���A�oAҡ�A���A�`BA��mA�^5A�"�Aϕ�AάA�"�A͛�A�I�A��A���A�Aˡ�A�n�A�ƨAɴ9AǏ\AƓuAēuA�|�A�$�A��;A���A��/A�oA�l�A��PA���A���A��mA���A��A���A��A��A�l�A�7LA��A�dZA��wA�ĜA��!A��A�r�A�;dA��A���A�C�A��DA��A�n�A�=qA�K�A��A�7LA��jA���A�ffA��A���A�K�A�-A���A��A�O�A�-A�{A�|�A�1'A�O�A��A|�jAx1'At�Ap�AmG�Aj�9AgK�Ad  A`�A]��A\=qAZ�9AV�AS��AR{AQdZAO|�AL �AIl�AF�AC��AC�wAB^5A@��A>VA;p�A:�A89XA7\)A7"�A6�jA41A/33A.�A-��A,��A+�7A*v�A(�A({A&r�A$�A#�TA"��A"ĜA"1'A ^5A�HAVA�DA��A��A�A^5A�wA\)A\)A�A?}At�AS�A"�AoA33AC�AoAVAƨA/A�AXAoA�`A�mAC�A��Ax�AȴAQ�A�FA
�A
�HA
~�A
ZA
��A�A�HAJA	�mA	l�A	;dA��AX@��7@�9X@��@���@�1'@�ff@�1'@�ȴ@�X@���@� �@�1@���@��@���@��@�E�@�O�@�~�@�33@���@�|�@�;d@���@�5?@�O�@�/@��@���@�O�@�|�@�@�F@�j@�A�@�R@���@��m@���@��T@�hs@���@�b@�(�@�1'@�z�@�j@�Z@�  @���@���@߾w@�t�@���@�@݉7@�bN@�o@�V@�@ى7@�V@�%@�Ĝ@�j@�I�@׶F@���@�ff@�=q@���@���@��@ёh@�1'@��
@��m@Ϯ@���@��@�%@�I�@��@���@�z�@��m@ǥ�@���@�n�@�5?@�p�@�(�@�5?@��^@���@�33@�
=@�A�@�l�@���@�&�@��9@�A�@�K�@�=q@���@�hs@��@��D@�bN@�A�@���@�|�@�dZ@�
=@�v�@���@��7@�7L@���@��D@�z�@�bN@��@��\@�M�@��T@�`B@�33@�v�@��@���@�&�@���@���@��`@�&�@�7L@��@���@��`@�Ĝ@��j@���@�bN@� �@�(�@� �@���@�t�@�\)@�@���@�p�@� �@�(�@�ƨ@�+@�"�@�"�@�33@���@��@���@�5?@���@�hs@�&�@��9@�1'@��@��
@���@��P@�t�@�K�@���@�=q@���@���@�X@�/@���@�9X@���@�t�@�+@�o@�@���@�~�@�M�@�=q@�$�@�J@��@�ff@�ff@�$�@�V@���@�|�@��H@���@�7L@���@���@��@���@���@��@�7L@��j@�A�@�+@��\@�^5@�5?@���@���@���@��@�Q�@��@��m@��m@��;@���@�+@��!@�^5@�X@��@�V@��j@�(�@�1'@�A�@�ƨ@�33@��y@�V@�J@���@��T@���@�x�@�7L@��@�%@��`@��/@��9@�(�@��@��@�bN@��@�bN@��@��;@�ƨ@��@�S�@�33@�"�@�o@���@��@��R@��R@��!@��\@�J@��^@��^@���@��7@�&�@��j@���@�r�@���@��;@�ƨ@��w@���@�\)@���@�ȴ@���@��R@��+@�p�@�[�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
jB
jB
iyB
jB
iyB
jB
iyB
jB
jB
iyB
jB
jB
jB
jB
jB
iyB
iyB
o�B
v�B
�uB
�B
�?B
�FB
�-B
�B
�B
��B
��B
��B
��B
��B
��B
�B
�B
�B
��B
�;B
�B
�B
��B\B/B=qBD�BE�BG�BJ�Bk�Bx�B~�B�bB�!B�qB��B��B��BbB<jB]/Bl�Bn�By�B�B�bB�JB�=B�DB��B~�Bu�BZBK�B@�BC�BW
BhsBcTBT�BO�Bx�Bq�BgmBx�Bt�Bl�B]/BE�B1'B$�B�BVB�mB��B��BŢB�3B�BS�B6FB%�B
��B
�B
�JB
s�B
]/B
;dB
�B
B	�B	�)B	��B	��B	�\B	t�B	cTB	O�B	?}B	/B	�B	VB	B��B�B�mB�NB�mB�B�`B��B�LB�LBÖB��BŢB��B�wB�}B�wB�jB�XB�3B�RB�XB�RB�LB�FB�3B�-B�!B�B�B�B��B��B��B�B�B�B�9B�LB��B��BÖBĜB��B��B��BǮB��BŢB��B�B�/B�NB�NB�`B�fB�ZB�5B�B�
B�)B�)B�#B�B��B��B�B�B�
B�5B�ZB�fB�B	�B	.B	'�B	�B	uB	\B��B�5B��B��B�/B�BB�BB�5B�B�B�B��B��B��B��B��B��B��B��B�#B�B	B	B	1B	
=B	JB	VB	VB	{B	�B	(�B	0!B	)�B	/B	9XB	J�B	M�B	L�B	H�B	J�B	J�B	H�B	F�B	F�B	Q�B	T�B	W
B	[#B	^5B	]/B	]/B	]/B	]/B	^5B	_;B	_;B	`BB	`BB	`BB	aHB	aHB	`BB	`BB	bNB	cTB	iyB	p�B	s�B	s�B	s�B	t�B	t�B	t�B	x�B	x�B	w�B	u�B	w�B	w�B	x�B	t�B	p�B	p�B	o�B	k�B	e`B	aHB	e`B	gmB	jB	q�B	s�B	q�B	q�B	p�B	n�B	m�B	n�B	q�B	}�B	� B	� B	� B	� B	~�B	�B	�B	�%B	�1B	�7B	�DB	�JB	�PB	�VB	�VB	�PB	�PB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�?B	�jB	�jB	�jB	�jB	�qB	�wB	�}B	�wB	�}B	B	ĜB	ĜB	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�BB	�HB	�ZB	�fB	�mB	�mB	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B
	7B
	7B
	7B

=B
	7B

=B
DB
bB
oB
hB
hB
hB
hB
hB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!B
(�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   B
jB
jB
iyB
jB
iyB
jB
iyB
jB
jB
iyB
jB
jB
jB
jB
jB
iyB
iyB
o�B
v�B
�uB
�B
�?B
�FB
�-B
�B
�B
��B
��B
��B
��B
��B
��B
�B
�B
�B
��B
�;B
�B
�B
��B\B/B=qBD�BE�BG�BJ�Bk�Bx�B~�B�bB�!B�qB��B��B��BbB<jB]/Bl�Bn�By�B�B�bB�JB�=B�DB��B~�Bu�BZBK�B@�BC�BW
BhsBcTBT�BO�Bx�Bq�BgmBx�Bt�Bl�B]/BE�B1'B$�B�BVB�mB��B��BŢB�3B�BS�B6FB%�B
��B
�B
�JB
s�B
]/B
;dB
�B
B	�B	�)B	��B	��B	�\B	t�B	cTB	O�B	?}B	/B	�B	VB	B��B�B�mB�NB�mB�B�`B��B�LB�LBÖB��BŢB��B�wB�}B�wB�jB�XB�3B�RB�XB�RB�LB�FB�3B�-B�!B�B�B�B��B��B��B�B�B�B�9B�LB��B��BÖBĜB��B��B��BǮB��BŢB��B�B�/B�NB�NB�`B�fB�ZB�5B�B�
B�)B�)B�#B�B��B��B�B�B�
B�5B�ZB�fB�B	�B	.B	'�B	�B	uB	\B��B�5B��B��B�/B�BB�BB�5B�B�B�B��B��B��B��B��B��B��B��B�#B�B	B	B	1B	
=B	JB	VB	VB	{B	�B	(�B	0!B	)�B	/B	9XB	J�B	M�B	L�B	H�B	J�B	J�B	H�B	F�B	F�B	Q�B	T�B	W
B	[#B	^5B	]/B	]/B	]/B	]/B	^5B	_;B	_;B	`BB	`BB	`BB	aHB	aHB	`BB	`BB	bNB	cTB	iyB	p�B	s�B	s�B	s�B	t�B	t�B	t�B	x�B	x�B	w�B	u�B	w�B	w�B	x�B	t�B	p�B	p�B	o�B	k�B	e`B	aHB	e`B	gmB	jB	q�B	s�B	q�B	q�B	p�B	n�B	m�B	n�B	q�B	}�B	� B	� B	� B	� B	~�B	�B	�B	�%B	�1B	�7B	�DB	�JB	�PB	�VB	�VB	�PB	�PB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�?B	�jB	�jB	�jB	�jB	�qB	�wB	�}B	�wB	�}B	B	ĜB	ĜB	ÖB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�/B	�5B	�BB	�HB	�ZB	�fB	�mB	�mB	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
	7B
	7B
	7B
	7B

=B
	7B

=B
DB
bB
oB
hB
hB
hB
hB
hB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!B
(�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190518                              AO  ARCAADJP                                                                    20181005190518    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190518  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190518  QCF$                G�O�G�O�G�O�8000            