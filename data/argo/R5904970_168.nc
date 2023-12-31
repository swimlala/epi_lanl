CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:32Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181024141532  20181024141532  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @��$�A��1   @��%$�"@8��+�d'���+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @,��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B��B  B   B(  B0  B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0�fD1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv�fDw  Dwl�Dy�3D�6D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @1�@~�R@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�B�BQ�B Q�B(Q�B0Q�B7�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�\)B�(�B�(�B���B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C.C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB.CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
=C�
=C�
=C�
=C�
=C�
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
=D D �DD�DD�DD�DD�DD�DD�D��D�DD�D	D	�D
D
��DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0��D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DE�DE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv��DwDwq�Dy�RD�8�D��\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A�7LA�=qA�?}A�33A�1'A�1'A�5?A�33A�33A��A��A��A�A��A��yA��TA��/A��A���A���AɮAɛ�Aɏ\A�x�A�ffA�O�A�5?A��A�1A�  A���A���A���A���A��`A��A���Aȥ�AȍPA�^5A�5?A�A�hsAƾwA�S�A���A���A���A��A�|�A�l�A�A��FA�A�E�A��A�"�A��;A�~�A�"�A��DA���A��A���A�VA��uA���A�dZA��wA��A��;A��RA�ZA���A�C�A�O�A�x�A�E�A�VA�bA�n�A�(�A�jA��RA�I�A�ȴA��9A���A��+A�l�A�C�A�+A�bA�9XA���A�-A�G�A��
A��RA���A�|�A�O�A�1A�hsA��A��A��A�"�A�bA�  A��A�
=A���A��A���A���A�hsA���A�`BA�VA�G�A}O�A|bA{�^Az�AxbAu�;As�TAsl�Aq��Ao��An�jAm��Ak+Ai�hAgt�Ae�Ad9XAa��A`��A_�7A]�;A\ffAZ�jAZbAY��AY7LAX�yAW�hAV5?AT��ASO�AQ��AO33AL��AJ��AH�jAE�wADȴAD9XAD  AA�A?t�A>bA=%A;�TA:��A8�RA6�\A5��A5��A4�RA3�PA1�TA1?}A0ZA/�wA/?}A-�
A,{A+XA*�RA*�A)"�A'ƨA'��A'��A'�A'�hA'C�A'A&bNA&�A%�-A#�FA"��A"n�A"I�A"5?A!oA�+A�HAA�A~�A�PA7LAJA+A�/A��AffA��Al�A
=A1'AdZA-A��A;dA��A��A=qAhsA{A33A��A�wA
�A	�hA�`AA�A��At�A�DA�hA ��@���@�ff@���@��7@�Ĝ@���@�9X@�ȴ@�O�@�P@�K�@��@���@���@�z�@�p�@��@�ff@�V@��@�p�@�j@�dZ@⟾@���@�x�@�/@���@��m@�@��@��/@۝�@���@�r�@�9X@���@��;@�ȴ@�v�@Չ7@�\)@��@�/@мj@�(�@϶F@�33@���@���@̬@̋D@�r�@�  @�+@�ff@ɩ�@��@��;@+@��@�/@��@�7L@��@�-@�@���@��7@�O�@�Ĝ@�9X@�33@���@���@� �@��
@��F@�C�@���@���@�\)@���@��T@�7L@��9@�Z@���@�33@��\@�^5@�5?@�$�@��#@��@�x�@�p�@�O�@��@�I�@�+@���@���@�/@��@��u@��@�Q�@���@�p�@�@�x�@�X@�?}@�%@��j@�  @� �@�(�@�  @��;@�r�@�9X@�1'@��
@���@�?}@��D@� �@���@�33@���@��@�ȴ@�^5@���@���@�V@��T@��#@���@��`@�O�@��@���@���@�r�@�j@�bN@�9X@��@��@� �@�Z@���@�A�@���@��@�K�@�+@�
=@�@���@��y@�ȴ@�ȴ@���@��R@���@�~�@�=q@���@��@�7L@�7L@���@��@��D@�j@�(�@��
@�\)@�+@�dZ@�\)@�l�@�I�@��@�j@��@��R@���@�ƨ@�\)@�o@��@��;@�9X@�t�@�E�@�\)@�S�@�;d@��-@��!@�M�@��@��#@���@�X@�/@�/@�&�@���@�Z@� �@��w@�\)@�+@��y@�M�@���@�O�@�/@��@���@���@�I�@�b@�  @��w@��;@��
@�S�@�o@���@�M�@���@�x�@�p�@�p�@��@�"�@�|�@�  @�1'@��@���@��;@��F@�\)@���@��@��7@��U@u�-@cC111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A�7LA�=qA�?}A�33A�1'A�1'A�5?A�33A�33A��A��A��A�A��A��yA��TA��/A��A���A���AɮAɛ�Aɏ\A�x�A�ffA�O�A�5?A��A�1A�  A���A���A���A���A��`A��A���Aȥ�AȍPA�^5A�5?A�A�hsAƾwA�S�A���A���A���A��A�|�A�l�A�A��FA�A�E�A��A�"�A��;A�~�A�"�A��DA���A��A���A�VA��uA���A�dZA��wA��A��;A��RA�ZA���A�C�A�O�A�x�A�E�A�VA�bA�n�A�(�A�jA��RA�I�A�ȴA��9A���A��+A�l�A�C�A�+A�bA�9XA���A�-A�G�A��
A��RA���A�|�A�O�A�1A�hsA��A��A��A�"�A�bA�  A��A�
=A���A��A���A���A�hsA���A�`BA�VA�G�A}O�A|bA{�^Az�AxbAu�;As�TAsl�Aq��Ao��An�jAm��Ak+Ai�hAgt�Ae�Ad9XAa��A`��A_�7A]�;A\ffAZ�jAZbAY��AY7LAX�yAW�hAV5?AT��ASO�AQ��AO33AL��AJ��AH�jAE�wADȴAD9XAD  AA�A?t�A>bA=%A;�TA:��A8�RA6�\A5��A5��A4�RA3�PA1�TA1?}A0ZA/�wA/?}A-�
A,{A+XA*�RA*�A)"�A'ƨA'��A'��A'�A'�hA'C�A'A&bNA&�A%�-A#�FA"��A"n�A"I�A"5?A!oA�+A�HAA�A~�A�PA7LAJA+A�/A��AffA��Al�A
=A1'AdZA-A��A;dA��A��A=qAhsA{A33A��A�wA
�A	�hA�`AA�A��At�A�DA�hA ��@���@�ff@���@��7@�Ĝ@���@�9X@�ȴ@�O�@�P@�K�@��@���@���@�z�@�p�@��@�ff@�V@��@�p�@�j@�dZ@⟾@���@�x�@�/@���@��m@�@��@��/@۝�@���@�r�@�9X@���@��;@�ȴ@�v�@Չ7@�\)@��@�/@мj@�(�@϶F@�33@���@���@̬@̋D@�r�@�  @�+@�ff@ɩ�@��@��;@+@��@�/@��@�7L@��@�-@�@���@��7@�O�@�Ĝ@�9X@�33@���@���@� �@��
@��F@�C�@���@���@�\)@���@��T@�7L@��9@�Z@���@�33@��\@�^5@�5?@�$�@��#@��@�x�@�p�@�O�@��@�I�@�+@���@���@�/@��@��u@��@�Q�@���@�p�@�@�x�@�X@�?}@�%@��j@�  @� �@�(�@�  @��;@�r�@�9X@�1'@��
@���@�?}@��D@� �@���@�33@���@��@�ȴ@�^5@���@���@�V@��T@��#@���@��`@�O�@��@���@���@�r�@�j@�bN@�9X@��@��@� �@�Z@���@�A�@���@��@�K�@�+@�
=@�@���@��y@�ȴ@�ȴ@���@��R@���@�~�@�=q@���@��@�7L@�7L@���@��@��D@�j@�(�@��
@�\)@�+@�dZ@�\)@�l�@�I�@��@�j@��@��R@���@�ƨ@�\)@�o@��@��;@�9X@�t�@�E�@�\)@�S�@�;d@��-@��!@�M�@��@��#@���@�X@�/@�/@�&�@���@�Z@� �@��w@�\)@�+@��y@�M�@���@�O�@�/@��@���@���@�I�@�b@�  @��w@��;@��
@�S�@�o@���@�M�@���@�x�@�p�@�p�@��@�"�@�|�@�  @�1'@��@���@��;@��F@�\)@���@��@��7@��U@u�-@cC111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B$�B&�B&�B$�B#�B#�B$�B(�B+B)�B)�B)�B.B0!B1'B1'B33B49B5?B7LB8RB:^B:^B:^B9XB9XB9XB9XB7LB7LB8RB8RB8RB8RB8RB8RB9XB;dB<jB>wB@�BB�BG�BG�BQ�B^5BaHBbNBdZBjBo�Bo�Bl�Bl�By�Bv�B� B��B��B��B��B��B�3B�-B��B��B��B��B��B�bB�VB�JB�DB�B�Bw�B^5BR�BC�B7LB/B"�BuBDB��B�yB�mB�fB�ZB�HB�5B�#B�B��B�!B��B�{B�oB�oB�oB�hB�bB�\B}�BbNBN�B.B�B�B�B
��B
�B
�B
�HB
��B
�jB
�B
��B
�uB
�JB
x�B
YB
I�B
E�B
>wB
&�B
�B
�B
�B
�B
B	��B	��B	�fB	�
B	��B	��B	�dB	�B	��B	��B	�1B	� B	p�B	m�B	jB	hsB	e`B	aHB	[#B	L�B	C�B	7LB	 �B	bB��B�B�B��BƨBĜB�jB�B�B��B��B��B��B�VB�DB�=B�7B�1B�1B�+B�B�B�B~�B�B~�B}�B{�Bx�Bw�By�By�B{�B}�B}�B}�B}�B|�B�B�1B�+B�+B�%B�%B�bB�DB�B}�Bw�Bn�Bl�BgmBbNB`BB^5B^5BZBYBYBXBW
BVBVBVBS�BR�BN�BL�BG�BE�BE�BE�BD�BA�B>wB8RB2-B.B+B%�B#�B"�B �B �B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B#�B&�B&�B&�B)�B+B)�B)�B(�B(�B(�B&�B(�B)�B)�B+B,B/B1'B49B5?B>wBH�BT�BXB^5B^5Be`Be`BcTBXBS�BW
B\)BaHBgmBiyBhsBhsBm�Bv�Bw�Bw�Bv�Bu�Bw�B�B�7B�7B�7B�JB�bB�\B�\B�VB�\B�\B�bB�hB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�XB�RB�qB�wB�}B��B��B�}BǮB��B��B��B��B��B��B��B��B��B�/B�`B�B�B�B�B�B�B�yB�yB�B�B�B�B��B��B	B	
=B	
=B	DB	PB	VB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	&�B	+B	.B	33B	7LB	8RB	;dB	;dB	<jB	=qB	A�B	D�B	F�B	G�B	H�B	L�B	O�B	P�B	R�B	VB	W
B	XB	[#B	\)B	]/B	]/B	_;B	cTB	ffB	jB	n�B	s�B	{�B	�B	�B	�1B	�1B	�\B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�-B	�3B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�3B	�3B	�?B	�FB	�LB	�RB	�^B	�^B	�^B	�^B	�^B	�^B	�XB	�XB	�XB	�^B	�jB	��B	��B	ĜB	ŢB	ǮB	ǮB	ǮB	ȴB	ǮB	ȴB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	ĶB	�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B$�B&�B&�B$�B#�B#�B$�B(�B+B)�B)�B)�B.B0!B1'B1'B33B49B5?B7LB8RB:^B:^B:^B9XB9XB9XB9XB7LB7LB8RB8RB8RB8RB8RB8RB9XB;dB<jB>wB@�BB�BG�BG�BQ�B^5BaHBbNBdZBjBo�Bo�Bl�Bl�By�Bv�B� B��B��B��B��B��B�3B�-B��B��B��B��B��B�bB�VB�JB�DB�B�Bw�B^5BR�BC�B7LB/B"�BuBDB��B�yB�mB�fB�ZB�HB�5B�#B�B��B�!B��B�{B�oB�oB�oB�hB�bB�\B}�BbNBN�B.B�B�B�B
��B
�B
�B
�HB
��B
�jB
�B
��B
�uB
�JB
x�B
YB
I�B
E�B
>wB
&�B
�B
�B
�B
�B
B	��B	��B	�fB	�
B	��B	��B	�dB	�B	��B	��B	�1B	� B	p�B	m�B	jB	hsB	e`B	aHB	[#B	L�B	C�B	7LB	 �B	bB��B�B�B��BƨBĜB�jB�B�B��B��B��B��B�VB�DB�=B�7B�1B�1B�+B�B�B�B~�B�B~�B}�B{�Bx�Bw�By�By�B{�B}�B}�B}�B}�B|�B�B�1B�+B�+B�%B�%B�bB�DB�B}�Bw�Bn�Bl�BgmBbNB`BB^5B^5BZBYBYBXBW
BVBVBVBS�BR�BN�BL�BG�BE�BE�BE�BD�BA�B>wB8RB2-B.B+B%�B#�B"�B �B �B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B �B �B#�B&�B&�B&�B)�B+B)�B)�B(�B(�B(�B&�B(�B)�B)�B+B,B/B1'B49B5?B>wBH�BT�BXB^5B^5Be`Be`BcTBXBS�BW
B\)BaHBgmBiyBhsBhsBm�Bv�Bw�Bw�Bv�Bu�Bw�B�B�7B�7B�7B�JB�bB�\B�\B�VB�\B�\B�bB�hB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�XB�RB�qB�wB�}B��B��B�}BǮB��B��B��B��B��B��B��B��B��B�/B�`B�B�B�B�B�B�B�yB�yB�B�B�B�B��B��B	B	
=B	
=B	DB	PB	VB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	&�B	+B	.B	33B	7LB	8RB	;dB	;dB	<jB	=qB	A�B	D�B	F�B	G�B	H�B	L�B	O�B	P�B	R�B	VB	W
B	XB	[#B	\)B	]/B	]/B	_;B	cTB	ffB	jB	n�B	s�B	{�B	�B	�B	�1B	�1B	�\B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�-B	�3B	�B	�B	�B	�B	�B	�!B	�'B	�3B	�3B	�3B	�?B	�FB	�LB	�RB	�^B	�^B	�^B	�^B	�^B	�^B	�XB	�XB	�XB	�^B	�jB	��B	��B	ĜB	ŢB	ǮB	ǮB	ǮB	ȴB	ǮB	ȴB	ǮB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	ĶB	�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141532                              AO  ARCAADJP                                                                    20181024141532    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141532  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141532  QCF$                G�O�G�O�G�O�0               