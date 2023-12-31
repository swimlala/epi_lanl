CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:46Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024140846  20181024140846  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @��$m�0 1   @��%�mP@5��v��d�E���1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B ffB  B  BffB   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<�C>�C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C��3C��3C��3C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3D � D  D�fD  D� D  D� DfD� D  Dy�D��D� D  D� D  D� D��D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!�fD"fD"� D#  D#� D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;�fD<  D<� D=  D=y�D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db�fDcfDc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dl��Dm� Dm��Dny�Do  Do� DpfDp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv�fDw  Dw� Dw� Dy�qD�F111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��\@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��
A��B �RBQ�BQ�B�RB Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�Bp�RBxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�\)B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C.C{C{C{C{C{C{C{C {C"{C${C&{C({C)��C,{C.{C0{C2{C4{C6{C8{C:{C<.C>.C@{CB{CD{CF{CH{CJ{CL{CN{CO��CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx.Cz{C|{C~{C�
=C�
=C�
=C�
=C�
C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C��pC�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C��pC��pC�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C�
=C�
=C�
=C��pC��pC�
=C�
=C�
=C�
=C�
=C��pC�
=C�
C�
C�
C�
=C�
=C�
=C�
=C��pC��pC��pC��pC��pC��pC��pC�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
C�
C�
C�
C�
C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
=C�
=C�
=C�
=C�
=C�
=C��pD �DD��DD�DD�D�D�DD~�D��D�DD�DD�D��D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�D��D�DD�DD�DD�DD�DD�DD�DD�DD�DD��DD�DD�D D �D!D!��D"�D"�D#D#�D/�D0D0�D1D1�D2D2�D3D3�D4D4��D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;��D<D<�D=D=~�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa��DbDb��Dc�Dc��DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�Dj�Dj�DkDk�DlDl�Dl��Dm�Dm��Dn~�DoDo�Dp�Dp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv��DwDw�Dw�Dy��D�H�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AÕ�AÓuA×�AÓuAÑhAÛ�Aß�Aß�Aß�Aß�Aá�Aã�Aã�Aå�Aç�Aç�Aç�Aé�Aé�AìAìAîAîAð!Að!Að!Aò-Aô9Aò-Aô9Aô9Aô9AöFAô9Aò-Aô9Aô9AöFAüjA�ƨA�1'A7A�E�A�A��FA�t�A��A���A��mA��A�dZA�A�A�$�A�"�A��/A��A�{A���A�33A�t�A���A�A�|�A�bA�A�A��A���A��wA���A��A�jA���A�C�A��A�JA��HA��PA�
=A���A��jA�r�A�7LA��jA�|�A�1'A�ƨA���A�S�A��A�1'A�Q�A�/A�O�A�|�A��A��PA��A�n�A���A���A��A� �A�ĜA��uA�|�A�p�A��A�A|~�Ay33AxbAv�\As&�Am��Ai��AeoA_��A]VA[AYK�AW��AVATȴAQ/APz�AO�AN�AL=qAI�AH{AF�ACx�AA��A?`BA>z�A;�FA:ZA8ȴA7��A6�/A4��A4�uA4jA4M�A3�FA21'A0�9A/�A-XA,�+A,bNA+�A*��A*5?A)�;A)O�A(9XA&ȴA%��A$�/A$E�A#��A#�7A"jA!��A!%A��Az�A��A�wA+AQ�A��A�Ar�A��A�AG�A��A�/A�A��A��AdZAv�AC�AoA�!A�+AAr�AƨA	dZA�-A�#AG�A^5AS�A�!A^5A1A�-A�@��@�A�@�dZ@��@�Ĝ@�z�@�(�@�E�@�@�"�@�@�@��@��@��@�$�@�Ĝ@�  @�w@�{@�%@�@�(�@�K�@���@�%@��@ߥ�@���@�n�@�=q@�@ܴ9@۾w@ۍP@�dZ@��@ڰ!@���@ٙ�@�%@�Ĝ@؃@ׅ@��@ԣ�@Ӿw@җ�@щ7@�Q�@��@̛�@�b@�;d@�@ʰ!@���@ɺ^@ə�@��@� �@�r�@�Q�@�G�@�?}@ȋD@���@ǶF@�;d@���@�V@��T@��@ÍP@��y@�n�@��#@��@��D@�9X@�b@��
@�S�@�O�@��y@�O�@��@��@��9@���@���@��@��@�Q�@�\)@�;d@���@��`@�r�@���@�o@���@�-@�&�@�`B@���@���@�\)@���@�(�@�Q�@��@���@��9@��`@�%@��@��`@��@�ƨ@��P@�K�@�V@���@�O�@���@��`@���@�Ĝ@��j@��@�bN@��@��@��P@�S�@�+@���@�ff@�V@�$�@�@��#@�hs@��@��/@�bN@��;@��@�|�@�S�@�o@��@�~�@��-@���@���@���@��
@�\)@�@��y@�K�@�
=@���@��\@��!@�ff@�+@���@�V@�/@�hs@��T@��@���@�G�@�/@�&�@��@���@�z�@��@�  @�K�@�+@�+@�33@�;d@�K�@�\)@�l�@�;d@���@���@�"�@�33@��@�o@��H@���@�ff@�V@�@���@���@�bN@�9X@���@��@��P@�|�@�t�@�S�@��@�o@���@��\@�n�@�-@�J@�@��h@��7@�p�@�O�@�7L@��`@��@�A�@���@��@��P@�\)@�
=@��H@�ȴ@��R@���@�n�@�5?@��@��-@���@�x�@�G�@�7L@��`@�b@��;@��P@���@�M�@��7@�hs@�/@�&�@�?}@�X@���@�Q�@�bN@�Ĝ@��@��/@���@��j@��@���@��@uϫ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  AÕ�AÓuA×�AÓuAÑhAÛ�Aß�Aß�Aß�Aß�Aá�Aã�Aã�Aå�Aç�Aç�Aç�Aé�Aé�AìAìAîAîAð!Að!Að!Aò-Aô9Aò-Aô9Aô9Aô9AöFAô9Aò-Aô9Aô9AöFAüjA�ƨA�1'A7A�E�A�A��FA�t�A��A���A��mA��A�dZA�A�A�$�A�"�A��/A��A�{A���A�33A�t�A���A�A�|�A�bA�A�A��A���A��wA���A��A�jA���A�C�A��A�JA��HA��PA�
=A���A��jA�r�A�7LA��jA�|�A�1'A�ƨA���A�S�A��A�1'A�Q�A�/A�O�A�|�A��A��PA��A�n�A���A���A��A� �A�ĜA��uA�|�A�p�A��A�A|~�Ay33AxbAv�\As&�Am��Ai��AeoA_��A]VA[AYK�AW��AVATȴAQ/APz�AO�AN�AL=qAI�AH{AF�ACx�AA��A?`BA>z�A;�FA:ZA8ȴA7��A6�/A4��A4�uA4jA4M�A3�FA21'A0�9A/�A-XA,�+A,bNA+�A*��A*5?A)�;A)O�A(9XA&ȴA%��A$�/A$E�A#��A#�7A"jA!��A!%A��Az�A��A�wA+AQ�A��A�Ar�A��A�AG�A��A�/A�A��A��AdZAv�AC�AoA�!A�+AAr�AƨA	dZA�-A�#AG�A^5AS�A�!A^5A1A�-A�@��@�A�@�dZ@��@�Ĝ@�z�@�(�@�E�@�@�"�@�@�@��@��@��@�$�@�Ĝ@�  @�w@�{@�%@�@�(�@�K�@���@�%@��@ߥ�@���@�n�@�=q@�@ܴ9@۾w@ۍP@�dZ@��@ڰ!@���@ٙ�@�%@�Ĝ@؃@ׅ@��@ԣ�@Ӿw@җ�@щ7@�Q�@��@̛�@�b@�;d@�@ʰ!@���@ɺ^@ə�@��@� �@�r�@�Q�@�G�@�?}@ȋD@���@ǶF@�;d@���@�V@��T@��@ÍP@��y@�n�@��#@��@��D@�9X@�b@��
@�S�@�O�@��y@�O�@��@��@��9@���@���@��@��@�Q�@�\)@�;d@���@��`@�r�@���@�o@���@�-@�&�@�`B@���@���@�\)@���@�(�@�Q�@��@���@��9@��`@�%@��@��`@��@�ƨ@��P@�K�@�V@���@�O�@���@��`@���@�Ĝ@��j@��@�bN@��@��@��P@�S�@�+@���@�ff@�V@�$�@�@��#@�hs@��@��/@�bN@��;@��@�|�@�S�@�o@��@�~�@��-@���@���@���@��
@�\)@�@��y@�K�@�
=@���@��\@��!@�ff@�+@���@�V@�/@�hs@��T@��@���@�G�@�/@�&�@��@���@�z�@��@�  @�K�@�+@�+@�33@�;d@�K�@�\)@�l�@�;d@���@���@�"�@�33@��@�o@��H@���@�ff@�V@�@���@���@�bN@�9X@���@��@��P@�|�@�t�@�S�@��@�o@���@��\@�n�@�-@�J@�@��h@��7@�p�@�O�@�7L@��`@��@�A�@���@��@��P@�\)@�
=@��H@�ȴ@��R@���@�n�@�5?@��@��-@���@�x�@�G�@�7L@��`@�b@��;@��P@���@�M�@��7@�hs@�/@�&�@�?}@�X@���@�Q�@�bN@�Ĝ@��@��/@���@��j@��@���@��@uϫ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B:^B:^B9XB:^B:^B:^B:^B:^B:^B:^B9XB:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B9XB:^B=qB[#Bn�B�B�bB�hB�{B��B��B�uB�bB�=B� B~�B~�B}�B~�B�VB��B�\B|�Bq�Bq�BjB@�B!�BVBB�B�B�yB�sB�mB�`B�HB�B��B�dB�RBB�dBĜB��B�!B��B��B��B��B��B�oB�VB�+Bz�BaHBL�B�BPBB
��B
��B
�yB
��B
��B
ƨB
�XB
��B
��B
��B
��B
��B
� B
m�B
T�B
I�B
9XB
�B	�/B	�FB	�JB	jB	T�B	K�B	C�B	8RB	+B	�B	+B	B��B�B�NB��B��BB�?B�B�B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�\B�VB�PB�DB�1B�B~�B{�Bz�Bx�Bw�Bv�Bu�Bu�Bv�Bv�Bu�Bt�Bs�Br�Br�Bq�Bm�Bq�Bs�Br�Bp�Bn�BiyBe`BcTB_;B[#BZBZB[#B\)B\)B^5B^5B^5B^5B^5B^5B_;BcTBe`BdZBgmBr�Bs�Bs�Bs�Bq�Bn�Bn�Bs�By�B{�B� B}�B|�Bz�By�Bw�Bv�Bv�Bw�Bx�Bx�By�By�By�B{�B�B�B�%B�7B�JB�{B��B��B��B��B��B��B�B�B�!B�'B�-B�'B�'B�3B�3B�9B�?B�^B�jB�qB�}B�}BÖBǮB��B��B�
B�B�B�B�B�B�
B�
B�B�)B�5B�HB�`B�sB�B�B�B�B�B��B�B�B�B�mB�B�B�B�yB�B�B�B�B�B�B�B�B�B��B��B��B	1B	�B	 �B	+B	1'B	33B	6FB	7LB	8RB	;dB	?}B	D�B	G�B	K�B	L�B	M�B	N�A�`BB	jB	l�B	n�B	o�B	p�B	p�B	p�B	q�B	s�B	v�B	x�B	y�B	{�B	|�B	�B	�B	�B	�%B	�1B	�7B	�JB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�?B	�9B	�?B	�RB	�dB	��B	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�;B	�BB	�TB	�`B	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
JB
PB
\B
hB
oB
oB
{B
aB
!�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B:^B:^B9XB:^B:^B:^B:^B:^B:^B:^B9XB:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B:^B9XB:^B=qB[#Bn�B�B�bB�hB�{B��B��B�uB�bB�=B� B~�B~�B}�B~�B�VB��B�\B|�Bq�Bq�BjB@�B!�BVBB�B�B�yB�sB�mB�`B�HB�B��B�dB�RBB�dBĜB��B�!B��B��B��B��B��B�oB�VB�+Bz�BaHBL�B�BPBB
��B
��B
�yB
��B
��B
ƨB
�XB
��B
��B
��B
��B
��B
� B
m�B
T�B
I�B
9XB
�B	�/B	�FB	�JB	jB	T�B	K�B	C�B	8RB	+B	�B	+B	B��B�B�NB��B��BB�?B�B�B��B��B��B��B��B��B��B��B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�uB�oB�hB�\B�VB�PB�DB�1B�B~�B{�Bz�Bx�Bw�Bv�Bu�Bu�Bv�Bv�Bu�Bt�Bs�Br�Br�Bq�Bm�Bq�Bs�Br�Bp�Bn�BiyBe`BcTB_;B[#BZBZB[#B\)B\)B^5B^5B^5B^5B^5B^5B_;BcTBe`BdZBgmBr�Bs�Bs�Bs�Bq�Bn�Bn�Bs�By�B{�B� B}�B|�Bz�By�Bw�Bv�Bv�Bw�Bx�Bx�By�By�By�B{�B�B�B�%B�7B�JB�{B��B��B��B��B��B��B�B�B�!B�'B�-B�'B�'B�3B�3B�9B�?B�^B�jB�qB�}B�}BÖBǮB��B��B�
B�B�B�B�B�B�
B�
B�B�)B�5B�HB�`B�sB�B�B�B�B�B��B�B�B�B�mB�B�B�B�yB�B�B�B�B�B�B�B�B�B��B��B��B	1B	�B	 �B	+B	1'B	33B	6FB	7LB	8RB	;dB	?}B	D�B	G�B	K�B	L�B	M�B	N�A�`BB	jB	l�B	n�B	o�B	p�B	p�B	p�B	q�B	s�B	v�B	x�B	y�B	{�B	|�B	�B	�B	�B	�%B	�1B	�7B	�JB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�?B	�9B	�?B	�RB	�dB	��B	ŢB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�;B	�BB	�TB	�`B	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
1B
JB
PB
\B
hB
oB
oB
{B
aB
!�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140846                              AO  ARCAADJP                                                                    20181024140846    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140846  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140846  QCF$                G�O�G�O�G�O�0               