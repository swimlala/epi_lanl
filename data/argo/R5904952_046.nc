CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:15Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190515  20181005190515  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               .A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @׺�z�d1   @׺�WM@1��`A�7�c�;dZ�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      .A   A   A   @�  @�  A��A   A>ffA`  A�  A�  A�  A�  A�  A���A���A���B   B  B  B  B   B(  B/��B7��B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CI�fCK�fCM�fCP  CR�CT�CV�CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Ci�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C��C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C��3C��3C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D  D� D  D� D  D� DfD� D  D� D��Dy�D��Dy�D  D� D	  D	y�D	��D
� DfD�fDfD�fD  D� D  D� D  D� D  D� DfD�fD  D� D��Dy�D��Dy�D��Dy�D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D fD y�D!  D!�fD"  D"� D#fD#�fD$  D$� D%  D%� D&  D&�fD'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-fD-� D-��D.y�D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7fD7� D8  D8� D9  D9� D:  D:�fD;  D;y�D<  D<� D=  D=y�D>  D>� D?  D?� D@  D@� DAfDA� DA��DB� DCfDC�fDD  DDy�DD��DE� DF  DF� DG  DG� DG��DH� DI  DIy�DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DNy�DN��DO� DP  DP� DQ  DQy�DQ��DRy�DR��DS� DT  DT� DU  DU� DV  DVy�DV��DWy�DX  DX� DY  DY� DZ  DZ� D[  D[�fD\fD\� D]  D]y�D^  D^� D_  D_� D`fD`�fDafDa� Db  Db�fDc  Dc� DdfDd� Dd��Dey�Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dj��Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dp��Dq� Dr  Dr� DsfDs� Dt  Dt� DufDu� Dv  Dv� Dv��Dwy�Dw��Dy��D�4{D��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@ǮAp�A#�
AB=pAc�
A��A��A��A��A��AҸRA�RA�RB ��B��B��B��B ��B(��B0�]B8�]B@��BH��BP��BX�]B`��Bh��Bp��Bx��B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��B�z�B�z�B�z�B�z�B�G�B�z�B�z�B�z�B�z�BЮBԮB�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C =qC=qC=qC#�C=qC
=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC=qC =qC"=qC$=qC&=qC(=qC*=qC,=qC.=qC0=qC2=qC4=qC6=qC8=qC:=qC<=qC>#�C@=qCB=qCD=qCF=qCH=qCJ#�CL#�CN#�CP=qCRWCTWCVWCX=qCZ#�C\=qC^=qC`=qCb=qCd=qCf=qCh=qCj#�Cl=qCn=qCp=qCr=qCt=qCv=qCx=qCz=qC|=qC~=qC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C�+�C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�+�C��C�+�C��C��C��C�+�C��C��C��C��C��C�+�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D �\D\D�\D\D�\D\D�\D�D�\D\D�\D�D��D�D��D\D�\D	\D	��D
�D
�\D�D��D�D��D\D�\D\D�\D\D�\D\D�\D�D��D\D�\D�D��D�D��D�D��D�D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D�\D\D��D�D�\D �D ��D!\D!��D"\D"�\D#�D#��D$\D$�\D%\D%�\D&\D&��D'\D'�\D(\D(��D)\D)�\D*\D*�\D+\D+�\D,\D,�\D-�D-�\D.�D.��D/\D/�\D0\D0�\D1\D1�\D2\D2�\D3\D3�\D4\D4�\D5\D5�\D6\D6��D7�D7�\D8\D8�\D9\D9�\D:\D:��D;\D;��D<\D<�\D=\D=��D>\D>�\D?\D?�\D@\D@�\DA�DA�\DB�DB�\DC�DC��DD\DD��DE�DE�\DF\DF�\DG\DG�\DH�DH�\DI\DI��DJ\DJ�\DK\DK�\DL\DL��DM\DM�\DN\DN��DO�DO�\DP\DP�\DQ\DQ��DR�DR��DS�DS�\DT\DT�\DU\DU�\DV\DV��DW�DW��DX\DX�\DY\DY�\DZ\DZ�\D[\D[��D\�D\�\D]\D]��D^\D^�\D_\D_�\D`�D`��Da�Da�\Db\Db��Dc\Dc�\Dd�Dd�\De�De��Df\Df�\Dg\Dg�\Dh\Dh��Di\Di�\Dj\Dj�\Dk�Dk�\Dl\Dl�\Dm\Dm�\Dn\Dn�\Do\Do�\Dp\Dp��Dq�Dq�\Dr\Dr�\Ds�Ds�\Dt\Dt�\Du�Du�\Dv\Dv�\Dw�Dw��Dw�)Dy��D�<)D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�`BA�`BA�bNA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�ffA�dZA�dZA�dZA�bNA�bNA�bNA�`BA�\)A�ZA�9XA�{A��HAլA՗�AՇ+A�C�A��A���A��;AԲ-A���A��HAԼjAԇ+A�z�A�n�A�XA�&�AӁA�K�AѼjA���A�  A�l�A�M�AɍPA�XA���A�M�Aǟ�A�(�A���A���A�ȴA�hsA�\)A��`AÁA�1'A�^5A��mA���A��A��A��A��\A�bA�t�A���A��A�&�A��A��uA�dZA�9XA��PA��A�-A���A���A�A�A�|�A�33A��A�oA���A�A��A�C�A��A�JA�VA���A�E�A�`BA��
A��A���A�r�A�9XA��A��#A�
=A��A�x�A�&�A�z�A��A���A�ȴA���A��^A���A| �AvQ�Au�hAtn�Ar=qAk��Ahz�AgoAe�Ab9XA^5?AZ��AYG�AX�+AW�AT��AR  AOXANJAM�PAMG�AL��AK�AI��AHQ�AF1AC|�AAO�A>�A>5?A<=qA;�7A;G�A:��A7��A3O�A0Q�A/S�A.��A.n�A-�^A+��A&�!A$bNA"��A!`BA 9XA;dA�A�RA�hA��A��A�HAM�A1'AVAffA%Ax�A��Ax�A��AJA;dA�A
=AVAJA��A33A`BA�wA�A	7LAE�A9XA��A�A��A�A��A�AG�A�!A�A A�@���@��h@��@�7L@��@�Q�@��@��+@�D@�  @�o@��@�bN@�
=@�G�@띲@��@�?}@�9@� �@��;@�K�@�v�@��/@��@��@�~�@◍@���@�5?@ᙚ@�33@��@ݡ�@�p�@݉7@��#@�@ۅ@ܣ�@�x�@ڟ�@ׅ@�Z@�9X@�\)@��@���@���@Ώ\@�{@�V@�1'@˶F@�C�@��#@�@���@ț�@���@���@��
@ǝ�@�o@ŉ7@�9X@î@�+@�dZ@��m@�z�@�
=@��-@���@��@�C�@���@�n�@�p�@�&�@�X@�{@��w@��@�@�-@��#@���@�O�@��@�b@�K�@�+@�ȴ@���@�n�@�-@��7@��@��@�%@�r�@��w@��@�;d@�+@��y@�n�@��@���@�hs@�V@�Q�@� �@���@���@�l�@�@���@�~�@��\@���@��+@�~�@�n�@�~�@��\@���@���@�?}@��@���@�\)@�E�@��@���@�?}@���@�  @��P@�\)@�o@��@���@�=q@��h@�?}@�7L@��@���@���@�9X@��F@�ȴ@�v�@���@�
=@�C�@��!@���@�5?@�{@��@�V@�ff@���@��!@��^@���@��/@���@���@��`@�Ĝ@��9@�z�@�ƨ@��@�S�@�33@�+@�33@��@��y@��@��\@��@�hs@��@���@���@�Z@��w@�"�@���@���@��@��@���@���@�^5@�$�@��@�`B@�5?@�E�@�J@��T@��^@���@�&�@��9@�z�@�Z@�bN@�j@�(�@�  @��
@�dZ@��@�~�@�V@�=q@�5?@�$�@��@��^@���@��7@�X@��@��@���@�I�@� �@�  @�|�@�ȴ@��+@�v�@�^5@�E�@�5?@��@��T@���@��-@���@�x�@�`B@�X@�X@�?}@�/@��@���@�I�@�K�@���@�ȴ@���@�~�@��@���@�?}@��@��`@��j@�Z@�1@�ƨ@���@�;d@��H@�M�@�O�@���@�j@�1'@��@�C�@��@��7@�X@��`@�I�@��@��R@��R@���@�$�@���@��@�� @��@q��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�`BA�`BA�bNA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�ffA�dZA�dZA�dZA�bNA�bNA�bNA�`BA�\)A�ZA�9XA�{A��HAլA՗�AՇ+A�C�A��A���A��;AԲ-A���A��HAԼjAԇ+A�z�A�n�A�XA�&�AӁA�K�AѼjA���A�  A�l�A�M�AɍPA�XA���A�M�Aǟ�A�(�A���A���A�ȴA�hsA�\)A��`AÁA�1'A�^5A��mA���A��A��A��A��\A�bA�t�A���A��A�&�A��A��uA�dZA�9XA��PA��A�-A���A���A�A�A�|�A�33A��A�oA���A�A��A�C�A��A�JA�VA���A�E�A�`BA��
A��A���A�r�A�9XA��A��#A�
=A��A�x�A�&�A�z�A��A���A�ȴA���A��^A���A| �AvQ�Au�hAtn�Ar=qAk��Ahz�AgoAe�Ab9XA^5?AZ��AYG�AX�+AW�AT��AR  AOXANJAM�PAMG�AL��AK�AI��AHQ�AF1AC|�AAO�A>�A>5?A<=qA;�7A;G�A:��A7��A3O�A0Q�A/S�A.��A.n�A-�^A+��A&�!A$bNA"��A!`BA 9XA;dA�A�RA�hA��A��A�HAM�A1'AVAffA%Ax�A��Ax�A��AJA;dA�A
=AVAJA��A33A`BA�wA�A	7LAE�A9XA��A�A��A�A��A�AG�A�!A�A A�@���@��h@��@�7L@��@�Q�@��@��+@�D@�  @�o@��@�bN@�
=@�G�@띲@��@�?}@�9@� �@��;@�K�@�v�@��/@��@��@�~�@◍@���@�5?@ᙚ@�33@��@ݡ�@�p�@݉7@��#@�@ۅ@ܣ�@�x�@ڟ�@ׅ@�Z@�9X@�\)@��@���@���@Ώ\@�{@�V@�1'@˶F@�C�@��#@�@���@ț�@���@���@��
@ǝ�@�o@ŉ7@�9X@î@�+@�dZ@��m@�z�@�
=@��-@���@��@�C�@���@�n�@�p�@�&�@�X@�{@��w@��@�@�-@��#@���@�O�@��@�b@�K�@�+@�ȴ@���@�n�@�-@��7@��@��@�%@�r�@��w@��@�;d@�+@��y@�n�@��@���@�hs@�V@�Q�@� �@���@���@�l�@�@���@�~�@��\@���@��+@�~�@�n�@�~�@��\@���@���@�?}@��@���@�\)@�E�@��@���@�?}@���@�  @��P@�\)@�o@��@���@�=q@��h@�?}@�7L@��@���@���@�9X@��F@�ȴ@�v�@���@�
=@�C�@��!@���@�5?@�{@��@�V@�ff@���@��!@��^@���@��/@���@���@��`@�Ĝ@��9@�z�@�ƨ@��@�S�@�33@�+@�33@��@��y@��@��\@��@�hs@��@���@���@�Z@��w@�"�@���@���@��@��@���@���@�^5@�$�@��@�`B@�5?@�E�@�J@��T@��^@���@�&�@��9@�z�@�Z@�bN@�j@�(�@�  @��
@�dZ@��@�~�@�V@�=q@�5?@�$�@��@��^@���@��7@�X@��@��@���@�I�@� �@�  @�|�@�ȴ@��+@�v�@�^5@�E�@�5?@��@��T@���@��-@���@�x�@�`B@�X@�X@�?}@�/@��@���@�I�@�K�@���@�ȴ@���@�~�@��@���@�?}@��@��`@��j@�Z@�1@�ƨ@���@�;d@��H@�M�@�O�@���@�j@�1'@��@�C�@��@��7@�X@��`@�I�@��@��R@��R@���@�$�@���@��@�� @��@q��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
Q�B
Q�B
Q�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
ZB
ffB
k�B
o�B
x�B
}�B
�B
�%B
�DB
��B
��B
�B
��B
��B
��B
�B
�?B
ŢB
��B
��B
��B
�
B
�mB
��B=qBiyB��B��B�B�B�B�B�B�?B�B+BbB�BM�BW
B\)Bn�B� B�bB��B�'B�XB�^B�dB�qB�jB�dB�dB�^B�XB�XB�jB�jB�FB�B��B�=B�Bz�Bp�BffBT�BA�B5?B&�B�B�BbBB��B�B`BB�B
�yB
��B
��B
��B
��B
�B
q�B
n�B
L�B
5?B
�B
�B
$�B
DB	�B	��B	��B	ƨB	�'B	�B	n�B	ffB	\)B	K�B	9XB	)�B	'�B	$�B	�B	uB	1B��B��B��B�B�B�B�fB�HB�#B��B��B��BȴBǮBŢBĜB��B�qB�XB�dB�dB�dB�^B�LB�'B��B��B��B��B��B��B�B�B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LB�RB�dB�LB�RB�3B�B�B�B�!B�3B�RB�}B�wB�dB�XB�RB�LB�FB�^B�qB�jB�wB�}B��B��B��B�}B��BBÖB��BĜBŢBƨBƨBǮB��BɺBɺB��B��B��B�
B�
B�B�5B�BB�TB�sB�B�B�B�B��B	B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B		7B	JB	VB	VB	VB	PB	VB	\B	oB	{B	�B	�B	"�B	%�B	#�B	�B	�B	�B	�B	�B	�B	!�B	&�B	.B	49B	@�B	^5B	ffB	hsB	hsB	hsB	iyB	o�B	q�B	s�B	w�B	~�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�7B	�PB	�PB	�VB	�\B	�\B	�bB	�hB	�hB	�hB	�oB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�3B	�-B	�3B	�3B	�-B	�?B	�^B	�}B	B	��B	��B	ŢB	ȴB	��B	��B	��B	�B	��B	��B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�TB	�TB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
1B
1B
+B
1B
+B
+B
+B
+B
%B
B
B
B
B
B
B
B
%B
%B
+B
+B
	7B
�B
�B
%222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B
Q�B
Q�B
Q�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
S�B
ZB
ffB
k�B
o�B
x�B
}�B
�B
�%B
�DB
��B
��B
�B
��B
��B
��B
�B
�?B
ŢB
��B
��B
��B
�
B
�mB
��B=qBiyB��B��B�B�B�B�B�B�?B�B+BbB�BM�BW
B\)Bn�B� B�bB��B�'B�XB�^B�dB�qB�jB�dB�dB�^B�XB�XB�jB�jB�FB�B��B�=B�Bz�Bp�BffBT�BA�B5?B&�B�B�BbBB��B�B`BB�B
�yB
��B
��B
��B
��B
�B
q�B
n�B
L�B
5?B
�B
�B
$�B
DB	�B	��B	��B	ƨB	�'B	�B	n�B	ffB	\)B	K�B	9XB	)�B	'�B	$�B	�B	uB	1B��B��B��B�B�B�B�fB�HB�#B��B��B��BȴBǮBŢBĜB��B�qB�XB�dB�dB�dB�^B�LB�'B��B��B��B��B��B��B�B�B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�LB�RB�dB�LB�RB�3B�B�B�B�!B�3B�RB�}B�wB�dB�XB�RB�LB�FB�^B�qB�jB�wB�}B��B��B��B�}B��BBÖB��BĜBŢBƨBƨBǮB��BɺBɺB��B��B��B�
B�
B�B�5B�BB�TB�sB�B�B�B�B��B	B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B		7B	JB	VB	VB	VB	PB	VB	\B	oB	{B	�B	�B	"�B	%�B	#�B	�B	�B	�B	�B	�B	�B	!�B	&�B	.B	49B	@�B	^5B	ffB	hsB	hsB	hsB	iyB	o�B	q�B	s�B	w�B	~�B	~�B	~�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�7B	�PB	�PB	�VB	�\B	�\B	�bB	�hB	�hB	�hB	�oB	�oB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�'B	�-B	�3B	�-B	�3B	�3B	�-B	�?B	�^B	�}B	B	��B	��B	ŢB	ȴB	��B	��B	��B	�B	��B	��B	�B	�#B	�)B	�/B	�/B	�5B	�;B	�;B	�;B	�BB	�HB	�TB	�TB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
1B
1B
1B
1B
1B
1B
+B
1B
+B
+B
+B
+B
%B
B
B
B
B
B
B
B
%B
%B
+B
+B
	7B
�B
�B
%222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.24 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190515                              AO  ARCAADJP                                                                    20181005190515    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190515  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190515  QCF$                G�O�G�O�G�O�8000            