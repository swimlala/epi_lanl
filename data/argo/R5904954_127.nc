CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:17Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005191717  20181005191717  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��%
�]1   @��%�Jf@4���R�dk"��`B1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffBffB  B ffB'��B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B���B���B���B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C�fC�fC
  C�C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C7�fC:  C<  C=�fC@  CB  CD  CF�CH33CJ�CL  CN  CP  CR  CT  CU�fCW�fCZ�C\  C^  C`�Cb  Cd  Cf�Ch  Cj  Cl  Cm�fCo�fCq�fCt  Cv�Cx  Cz  C|  C~  C�fC��3C�  C��C�  C�  C��C��C�  C��fC��3C�  C�  C�  C��3C�  C��C�  C�  C�  C��C�  C��3C��3C�  C�  C��3C��C��C��C��C�  C�  C��C�  C��3C��C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C��C�  C��C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C��fC��3C��3C�  C��C�  C��3C��3C�  C�  C��fC��3C�  C�  C��3C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C��3C�  C�  C�  C�  D   D � D ��D� D��D� DfD� D��D� D��D� D  D�fDfD�fDfD�fD	fD	� D	��D
y�D  D� D��Dy�D  D�fD  Dy�D��D� D��Dy�D  D� D��D� DfD�fDfD�fDfD�fD  Dy�D��D� DfD� D  D� D  D� D��D�fDfD�fDfD� D  D� D  D� D   D y�D!  D!��D"fD"� D"��D#y�D#��D$y�D$��D%� D%��D&� D'  D'� D(  D(�fD)fD)� D)��D*� D+fD+� D,  D,�fD-fD-�fD.  D.�fD/fD/� D0  D0� D0��D1y�D2  D2� D3  D3� D4  D4� D5fD5�fD6  D6y�D6��D7y�D7��D8s3D9  D9�fD:  D:� D;  D;� D<  D<�fD=  D=y�D=��D>y�D>��D?� D@fD@� DA  DA� DB  DB� DCfDC�fDDfDD� DD��DE� DF  DFs3DF�3DGy�DHfDH�fDIfDI� DJ  DJ�fDK  DK� DK��DL�fDM  DMy�DN  DNs3DN��DO�fDP  DP�fDQ  DQy�DRfDR� DR��DSy�DS��DT� DU  DUy�DV  DV� DW  DW�fDX  DX� DX��DY�fDZfDZ�fD[fD[� D\fD\� D]  D]�fD^  D^y�D^��D_y�D`  D`� D`��Day�Db  Db� DcfDc�fDdfDdy�Dd��De� De��Df� DgfDg�fDh  Dhy�Di  Di� Di��Dj�fDk  Dk� Dl  Dl�fDmfDm�fDn  Dny�Do  Do�fDpfDp� DqfDq�fDr  Dr� Dr��Ds� Dt  Dt� Du  Du� Dv  Dvy�Dv��Dwy�Dw�fDy��D�H�D��q1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @B�\@��@ǮA�
A#�
AC�
Ac�
A��A��A��A��A��A��A��A��B ��B	\)B\)B��B!\)B(�]B0�]B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�z�B�z�B�z�B�z�B��B�{B�G�B�G�B�G�B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�BخB�z�B�z�B�z�B�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC#�C#�C
=qCWC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.#�C0=qC2=qC4=qC6=qC8#�C:=qC<=qC>#�C@=qCB=qCD=qCFWCHp�CJWCL=qCN=qCP=qCR=qCT=qCV#�CX#�CZWC\=qC^=qC`WCb=qCd=qCfWCh=qCj=qCl=qCn#�Cp#�Cr#�Ct=qCvWCx=qCz=qC|=qC~=qC��C��C��C�+�C��C��C�+�C�+�C��C�C��C��C��C��C��C��C�+�C��C��C��C�+�C��C��C��C��C��C��C�+�C�+�C�+�C�+�C��C��C�8RC��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C�+�C��C��C��C��C��C��C��C�+�C��C��C��C��C��C��C�+�C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C�+�C��C��C��C��C��C�C��C��C��C��C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C��C��C��C��C��C��D \D �\D�D�\D�D�\D�D�\D�D�\D�D�\D\D��D�D��D�D��D	�D	�\D
�D
��D\D�\D�D��D\D��D\D��D�D�\D�D��D\D�\D�D�\D�D��D�D��D�D��D\D��D�D�\D�D�\D\D�\D\D�\D�D��D�D��D�D�\D\D�\D\D�\D \D ��D!\D!�)D"�D"�\D#�D#��D$�D$��D%�D%�\D&�D&�\D'\D'�\D(\D(��D)�D)�\D*�D*�\D+�D+�\D,\D,��D-�D-��D.\D.��D/�D/�\D0\D0�\D1�D1��D2\D2�\D3\D3�\D4\D4�\D5�D5��D6\D6��D7�D7��D8�D8��D9\D9��D:\D:�\D;\D;�\D<\D<��D=\D=��D>�D>��D?�D?�\D@�D@�\DA\DA�\DB\DB�\DC�DC��DD�DD�\DE�DE�\DF\DF��DG�DG��DH�DH��DI�DI�\DJ\DJ��DK\DK�\DL�DL��DM\DM��DN\DN��DO�DO��DP\DP��DQ\DQ��DR�DR�\DS�DS��DT�DT�\DU\DU��DV\DV�\DW\DW��DX\DX�\DY�DY��DZ�DZ��D[�D[�\D\�D\�\D]\D]��D^\D^��D_�D_��D`\D`�\Da�Da��Db\Db�\Dc�Dc��Dd�Dd��De�De�\Df�Df�\Dg�Dg��Dh\Dh��Di\Di�\Dj�Dj��Dk\Dk�\Dl\Dl��Dm�Dm��Dn\Dn��Do\Do��Dp�Dp�\Dq�Dq��Dr\Dr�\Ds�Ds�\Dt\Dt�\Du\Du�\Dv\Dv��Dw�Dw��Dw��Dy��D�PRD��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�+A�5?A�;dA�A�A�C�A�I�A�K�A�K�A�K�A�M�A�O�A�M�A�Q�A�Q�A�M�A�(�A���A۰!A۝�A�~�Aٟ�Aԗ�A�n�A�33A˼jA�ȴAƗ�A��Aţ�A�`BA�$�A�A��FA��!A��A��A�O�A�JA�VA���A��A��jA���A��A�=qA�1A��`A�O�A��jA��A��/A�l�A�=qA�|�A��A�XA��-A��PA�ĜA�K�A���A�;dA��A��yA�ĜA�&�A���A��A�1A��HA� �A�t�A���A���A�33A��jA�9XA��A�ƨA���A�1A�
=A��hA��wA�
=AA}��A|�A{dZAx �Au%AqdZAl�Ak�hAiVAd�9AcoAb=qA^�`A^-A]A\�A\{A[t�A[+AZ~�AY+AW��AV��AU�PASXAR1'AP�AM��AJ��AIXAH-AFv�AEXAD  ABr�A@I�A>�A=+A9��A8��A8A�A6^5A5&�A4jA2ĜA0�A/%A.1'A-\)A,�jA,�A+��A*��A)A(VA't�A'7LA&�A& �A%�A$9XA"��A!"�A7LAv�A�A\)A�HAjA��A\)A;dAĜA �A�mA��A�^A?}AffAbA�TAl�A��AJA��AM�A;dA��AoAv�A  A\)A-A
=A�A$�A�At�A
�A
��A
��A
��A	��A-A�mA�hA�A��A�wA`BA~�A|�@�dZ@��j@���@��^@� �@�M�@�A�@�hs@�  @�^5@�=q@�z�@��@���@��@�"�@���@�z�@�dZ@���@�$�@ٙ�@�V@Չ7@��@�(�@�1@��@�{@ļj@�r�@�+@�v�@�x�@��@���@�j@��@�^5@��^@���@�^5@��7@�Ĝ@�Q�@�dZ@���@��@���@���@�Z@��@�A�@���@��\@�E�@���@��;@�t�@�33@��@��!@��7@��@�9X@��@��+@�M�@�-@��#@�/@�S�@���@�V@�=q@��T@�Ĝ@��P@�+@�o@�
=@���@��H@��!@�~�@�V@��@�@��^@�hs@�%@��/@��u@�Ĝ@���@���@��/@�O�@��-@�E�@��m@�|�@���@��@�l�@���@�n�@��-@�hs@�&�@�z�@��
@�C�@�"�@��!@�$�@�{@���@�$�@���@�E�@��7@�`B@�G�@���@���@�X@�x�@�I�@��m@���@� �@��
@���@��
@���@��;@�|�@���@���@�~�@�M�@���@���@�x�@�hs@�hs@�@��@�j@� �@� �@�Q�@��@�/@�Ĝ@��D@� �@�  @�(�@�ƨ@��@���@�{@�?}@�&�@�%@��@�9X@���@���@�ff@�=q@��@�@�hs@���@��D@�9X@�I�@�1'@��@��w@��@�"�@��y@���@���@��\@��+@�v�@�ff@�=q@��@�{@�J@��#@��^@��-@��-@���@���@��@�O�@�X@��@���@�@�@�/@�j@�A�@��@�j@�  @��F@�+@���@�ȴ@���@���@���@��y@��@���@�{@��@��j@��D@�z�@�bN@��@�t�@�33@�dZ@��P@�t�@�\)@�C�@���@��F@��y@�ff@�^5@�{@���@���@���@��h@�p�@�%@�/@���@�hs@���@�E�@��#@�ȴ@���@��!@��\@�v�@�=q@�@���@���@��@��#@���@�p�@�G�@�&�@��@���@�Ĝ@���@��9@�z�@�Z@�A�@���@��m@��;@�|�@�l�@�dZ@�dZ@�S�@�
=@���@��^@���@���@���@���@�j@��F@��P@���@m��@[�g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�+A�5?A�;dA�A�A�C�A�I�A�K�A�K�A�K�A�M�A�O�A�M�A�Q�A�Q�A�M�A�(�A���A۰!A۝�A�~�Aٟ�Aԗ�A�n�A�33A˼jA�ȴAƗ�A��Aţ�A�`BA�$�A�A��FA��!A��A��A�O�A�JA�VA���A��A��jA���A��A�=qA�1A��`A�O�A��jA��A��/A�l�A�=qA�|�A��A�XA��-A��PA�ĜA�K�A���A�;dA��A��yA�ĜA�&�A���A��A�1A��HA� �A�t�A���A���A�33A��jA�9XA��A�ƨA���A�1A�
=A��hA��wA�
=AA}��A|�A{dZAx �Au%AqdZAl�Ak�hAiVAd�9AcoAb=qA^�`A^-A]A\�A\{A[t�A[+AZ~�AY+AW��AV��AU�PASXAR1'AP�AM��AJ��AIXAH-AFv�AEXAD  ABr�A@I�A>�A=+A9��A8��A8A�A6^5A5&�A4jA2ĜA0�A/%A.1'A-\)A,�jA,�A+��A*��A)A(VA't�A'7LA&�A& �A%�A$9XA"��A!"�A7LAv�A�A\)A�HAjA��A\)A;dAĜA �A�mA��A�^A?}AffAbA�TAl�A��AJA��AM�A;dA��AoAv�A  A\)A-A
=A�A$�A�At�A
�A
��A
��A
��A	��A-A�mA�hA�A��A�wA`BA~�A|�@�dZ@��j@���@��^@� �@�M�@�A�@�hs@�  @�^5@�=q@�z�@��@���@��@�"�@���@�z�@�dZ@���@�$�@ٙ�@�V@Չ7@��@�(�@�1@��@�{@ļj@�r�@�+@�v�@�x�@��@���@�j@��@�^5@��^@���@�^5@��7@�Ĝ@�Q�@�dZ@���@��@���@���@�Z@��@�A�@���@��\@�E�@���@��;@�t�@�33@��@��!@��7@��@�9X@��@��+@�M�@�-@��#@�/@�S�@���@�V@�=q@��T@�Ĝ@��P@�+@�o@�
=@���@��H@��!@�~�@�V@��@�@��^@�hs@�%@��/@��u@�Ĝ@���@���@��/@�O�@��-@�E�@��m@�|�@���@��@�l�@���@�n�@��-@�hs@�&�@�z�@��
@�C�@�"�@��!@�$�@�{@���@�$�@���@�E�@��7@�`B@�G�@���@���@�X@�x�@�I�@��m@���@� �@��
@���@��
@���@��;@�|�@���@���@�~�@�M�@���@���@�x�@�hs@�hs@�@��@�j@� �@� �@�Q�@��@�/@�Ĝ@��D@� �@�  @�(�@�ƨ@��@���@�{@�?}@�&�@�%@��@�9X@���@���@�ff@�=q@��@�@�hs@���@��D@�9X@�I�@�1'@��@��w@��@�"�@��y@���@���@��\@��+@�v�@�ff@�=q@��@�{@�J@��#@��^@��-@��-@���@���@��@�O�@�X@��@���@�@�@�/@�j@�A�@��@�j@�  @��F@�+@���@�ȴ@���@���@���@��y@��@���@�{@��@��j@��D@�z�@�bN@��@�t�@�33@�dZ@��P@�t�@�\)@�C�@���@��F@��y@�ff@�^5@�{@���@���@���@��h@�p�@�%@�/@���@�hs@���@�E�@��#@�ȴ@���@��!@��\@�v�@�=q@�@���@���@��@��#@���@�p�@�G�@�&�@��@���@�Ĝ@���@��9@�z�@�Z@�A�@���@��m@��;@�|�@�l�@�dZ@�dZ@�S�@�
=@���@��^@���@���@���@���@�j@��F@��P@���@m��@[�g1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
=B
=B
=B
=B
=B	7B
=B
=B
=BDBJBJBPB\BuB#�BC�BK�BL�BL�BG�B=qB=qB9XB<jB[#BdZBgmBk�Bl�Bl�Br�B�B�JB�oB��B��B��B�dBÖBȴBɺBĜB�XB�!B��B��B�oB�JB�=B�%B{�Bo�BgmBT�B?}B6FB%�B�BhB
=BB�B�TB�5B��B�!B�VB}�BaHBR�B/BJB
��B
��B
�B
�fB
�NB
�5B
�
B
�?B
�DB
iyB
O�B
E�B
=qB
5?B
-B
!�B

=B	��B	�/B	ÖB	�RB	�B	��B	�JB	�%B	s�B	o�B	iyB	ffB	cTB	_;B	]/B	XB	P�B	I�B	C�B	=qB	1'B	)�B	�B	uB	+B��B��B�B�B�ZB�;B�B�B�B��B��B��BĜB��B�wB�XB�RB�?B�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�oB�hB�bB�\B�VB�PB�JB�7B�B� B� B�B�B�B�+B�7B�=B�=B�=B�=B�=B�=B�=B�7B�7B�+B�B�B�B�B�B� B}�B|�Bv�Bt�Bt�Br�Br�Bs�Bs�Bo�BjBe`B\)BW
BS�BF�BG�BI�BP�BZBYBXBXBW
BT�BM�BF�BA�B?}B=qBA�BB�BC�BE�BF�BH�BH�BH�BH�BL�BL�BN�BT�BW
BXBYBZB]/B^5B^5BffBiyBm�Br�Bt�By�B~�B� B�B�B�B�%B�+B�+B�DB�JB�VB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�?B�^B�wB��BĜB��B��B�
B�NB�B�B��B��B��B	PB	\B	hB	oB	uB	�B	�B	"�B	#�B	&�B	)�B	+B	,B	,B	,B	.B	0!B	6FB	6FB	5?B	;dB	;dB	<jB	<jB	;dB	<jB	?}B	B�B	F�B	E�B	E�B	J�B	M�B	M�B	M�B	T�B	ZB	[#B	ZB	ZB	ZB	ZB	]/B	^5B	^5B	_;B	cTB	k�B	u�B	~�B	� B	�B	�B	�1B	�=B	�=B	�7B	�7B	�1B	�=B	�VB	�bB	�\B	�PB	�DB	�DB	�JB	�JB	�\B	�\B	�VB	�\B	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�?B	�FB	�FB	�LB	�FB	�LB	�^B	�dB	�dB	�qB	�wB	��B	��B	��B	��B	��B	ÖB	ÖB	ÖB	��B	��B	��B	B	ÖB	ÖB	ÖB	��B	��B	ÖB	ŢB	ŢB	ŢB	ŢB	ǮB	ɺB	ɺB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�;B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
1B
1B
1B
1B
1B
B
B
B
B
+B
+B
%B
%B
%B
&B
!2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
=B
=B
=B
=B
=B	7B
=B
=B
=BDBJBJBPB\BuB#�BC�BK�BL�BL�BG�B=qB=qB9XB<jB[#BdZBgmBk�Bl�Bl�Br�B�B�JB�oB��B��B��B�dBÖBȴBɺBĜB�XB�!B��B��B�oB�JB�=B�%B{�Bo�BgmBT�B?}B6FB%�B�BhB
=BB�B�TB�5B��B�!B�VB}�BaHBR�B/BJB
��B
��B
�B
�fB
�NB
�5B
�
B
�?B
�DB
iyB
O�B
E�B
=qB
5?B
-B
!�B

=B	��B	�/B	ÖB	�RB	�B	��B	�JB	�%B	s�B	o�B	iyB	ffB	cTB	_;B	]/B	XB	P�B	I�B	C�B	=qB	1'B	)�B	�B	uB	+B��B��B�B�B�ZB�;B�B�B�B��B��B��BĜB��B�wB�XB�RB�?B�3B�'B�B�B�B��B��B��B��B��B��B��B��B��B�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�oB�hB�bB�\B�VB�PB�JB�7B�B� B� B�B�B�B�+B�7B�=B�=B�=B�=B�=B�=B�=B�7B�7B�+B�B�B�B�B�B� B}�B|�Bv�Bt�Bt�Br�Br�Bs�Bs�Bo�BjBe`B\)BW
BS�BF�BG�BI�BP�BZBYBXBXBW
BT�BM�BF�BA�B?}B=qBA�BB�BC�BE�BF�BH�BH�BH�BH�BL�BL�BN�BT�BW
BXBYBZB]/B^5B^5BffBiyBm�Br�Bt�By�B~�B� B�B�B�B�%B�+B�+B�DB�JB�VB�oB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�?B�^B�wB��BĜB��B��B�
B�NB�B�B��B��B��B	PB	\B	hB	oB	uB	�B	�B	"�B	#�B	&�B	)�B	+B	,B	,B	,B	.B	0!B	6FB	6FB	5?B	;dB	;dB	<jB	<jB	;dB	<jB	?}B	B�B	F�B	E�B	E�B	J�B	M�B	M�B	M�B	T�B	ZB	[#B	ZB	ZB	ZB	ZB	]/B	^5B	^5B	_;B	cTB	k�B	u�B	~�B	� B	�B	�B	�1B	�=B	�=B	�7B	�7B	�1B	�=B	�VB	�bB	�\B	�PB	�DB	�DB	�JB	�JB	�\B	�\B	�VB	�\B	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�9B	�?B	�FB	�FB	�LB	�FB	�LB	�^B	�dB	�dB	�qB	�wB	��B	��B	��B	��B	��B	ÖB	ÖB	ÖB	��B	��B	��B	B	ÖB	ÖB	ÖB	��B	��B	ÖB	ŢB	ŢB	ŢB	ŢB	ǮB	ɺB	ɺB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�#B	�;B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
+B
1B
1B
1B
1B
1B
B
B
B
B
+B
+B
%B
%B
%B
&B
!2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191717                              AO  ARCAADJP                                                                    20181005191717    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191717  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191717  QCF$                G�O�G�O�G�O�8000            