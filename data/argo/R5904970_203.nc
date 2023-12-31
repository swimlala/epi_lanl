CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  	   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:40Z creation      
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
resolution        =���   axis      Z        $  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     $  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ^   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  `$   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   hH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  jT   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  rx   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  |�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     $  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �,   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �,   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �,   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �,   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141540  20181024141540  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @���O՗�1   @����H!^@7�ě��T�c� ě��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �B   B   B   @333@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B��B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|�C~  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D.  D.� D/fD/� D0  D0� D1  D1� D2  D2� D3  D3�fD4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D<��D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db�fDcfDc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw�3Dy��D�9�D��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @8Q�@��\@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�B�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�B�(�B�(�B�(�B�\)B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C{C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cw��Cz{C|.C~{C�
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
=C��pC�
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
=D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/�D/�D0D0�D1D1�D2D2�D3D3��D4D4�D5D5�D6D6�D7D7�D8D8�D.D.�D/�D/�D0D0�D1D1�D2D2�D3D3��D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D<��D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD��DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DU��DV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb��Dc�Dc��DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�Dw�RDy��D�<{D�=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��9A��-A���A��jA���A���A���A���A�ƨA��A� �A��^A��#A���A�ĜA´9A�A7A�z�A�`BA��A�JA�A���A��wA���A�`BA�K�A�C�A�=qA�1'A���A��A���A�t�A�7LA��
A���A�VA�C�A�=qA�7LA�1'A�$�A��A�
=A��;A���A�  A�K�A�v�A���A�5?A�+A�dZA�"�A�7LA�K�A���A�ȴA��A�33A�VA�9XA��!A��A�VA�/A���A��DA�5?A��^A�  A�n�A��7A���A�JA�~�A���A��mA��A�G�A���A�=qA��\A��A�O�A���A��!A���A�G�A��;A��RA��HA���A���A�Q�A���A���A��A�;dA�
=A��7A�VA���A�M�A��A���A�1'A���AS�A}33A{|�Az�Az�Ax�\Aw�Av�DAt�DAr��Ar(�ArbAq�mAq�Ao�-An{Al��Akt�Aj^5AiAg|�Ag"�Ae��AdQ�Ac�-Ac�PAcl�Ab��Aa�FA_hsA^E�A]��A]hsA\�A[�7A[\)A[G�A[+AZĜAZ=qAX��AW�7AU�AT-AR��AQx�AOS�ANbAL1'AK|�AJ�\AH~�AG%AF�+AE��AC�-ACoABM�AA\)AA&�A@�HA@ffA?�-A>��A>5?A<��A:��A:{A8�/A7�-A7"�A6~�A5�wA4��A4�+A3�
A3�A2��A2Q�A1O�A0��A/��A.^5A-7LA,A�A,1A*�A)l�A(9XA'K�A&M�A%A%A$��A#t�A"ZA!C�A�uAv�AE�AhsA{A$�A/A�^A��A7LA;dA�A�HA-AG�A^5A&�A�uAS�A	�hAjA��A��AS�AffA�PAdZA/AjAA/A��Al�A ��A 5?@�dZ@�v�@�{@���@��/@�@���@�b@�@�h@��@��@�h@�b@�!@�z�@�bN@�O�@�@�7L@�Ĝ@��m@�Ĝ@�9X@�D@�@���@�^5@���@�V@��@�Z@�"�@��@�~�@�/@ݡ�@�r�@ܓu@�j@�l�@� �@�(�@�+@�v�@�r�@щ7@�$�@��T@ѡ�@мj@��m@�G�@���@̴9@̃@�Q�@�=q@���@���@���@őh@��/@�@��9@���@�@�bN@�@���@���@�~�@��7@��F@�+@��@�C�@�K�@�o@���@�z�@�1@�(�@���@�33@�^5@���@��@��j@��@�|�@�K�@�l�@��@��#@��-@��^@�J@���@�{@���@��@��P@��H@�n�@��@���@��7@�X@�?}@��@�j@�o@�E�@���@��7@�V@�bN@�7L@�O�@�%@�?}@�x�@��w@�\)@��;@�t�@��@���@��7@�X@�?}@��@�j@�o@�E�@���@��7@�V@�bN@�7L@�O�@�%@�?}@�x�@��w@�\)@��;@�t�@�r�@��^@��H@�ff@���@�@��\@��@��y@��R@�ȴ@��H@��H@���@��\@�^5@��@�Ĝ@�ƨ@�S�@�@��!@�ȴ@���@�=q@���@��/@��
@��\@�$�@��R@��
@���@���@�|�@���@�-@�p�@�1'@���@�+@��P@��
@�ƨ@�(�@�Q�@��w@�l�@���@��F@���@���@��;@��@��\@��^@��h@�O�@��@��@��9@�bN@��@���@��@���@��@���@��@�|�@�l�@�o@���@�^5@��#@�O�@�G�@�7L@�V@���@��9@��u@�bN@���@�S�@�33@�
=@���@�M�@��@���@���@�G�@���@�bN@�I�@�9X@�1@���@�C�@��@���@�^5@�5?@�@��@�@���@�E�@�~�@���@��@�o@�33@���@��R@�~�@�v�@�M�@�=q@�E�@�V@�^5@�^5@�^5@���@�/@�"h@x��@i��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��9A��-A���A��jA���A���A���A���A�ƨA��A� �A��^A��#A���A�ĜA´9A�A7A�z�A�`BA��A�JA�A���A��wA���A�`BA�K�A�C�A�=qA�1'A���A��A���A�t�A�7LA��
A���A�VA�C�A�=qA�7LA�1'A�$�A��A�
=A��;A���A�  A�K�A�v�A���A�5?A�+A�dZA�"�A�7LA�K�A���A�ȴA��A�33A�VA�9XA��!A��A�VA�/A���A��DA�5?A��^A�  A�n�A��7A���A�JA�~�A���A��mA��A�G�A���A�=qA��\A��A�O�A���A��!A���A�G�A��;A��RA��HA���A���A�Q�A���A���A��A�;dA�
=A��7A�VA���A�M�A��A���A�1'A���AS�A}33A{|�Az�Az�Ax�\Aw�Av�DAt�DAr��Ar(�ArbAq�mAq�Ao�-An{Al��Akt�Aj^5AiAg|�Ag"�Ae��AdQ�Ac�-Ac�PAcl�Ab��Aa�FA_hsA^E�A]��A]hsA\�A[�7A[\)A[G�A[+AZĜAZ=qAX��AW�7AU�AT-AR��AQx�AOS�ANbAL1'AK|�AJ�\AH~�AG%AF�+AE��AC�-ACoABM�AA\)AA&�A@�HA@ffA?�-A>��A>5?A<��A:��A:{A8�/A7�-A7"�A6~�A5�wA4��A4�+A3�
A3�A2��A2Q�A1O�A0��A/��A.^5A-7LA,A�A,1A*�A)l�A(9XA'K�A&M�A%A%A$��A#t�A"ZA!C�A�uAv�AE�AhsA{A$�A/A�^A��A7LA;dA�A�HA-AG�A^5A&�A�uAS�A	�hAjA��A��AS�AffA�PAdZA/AjAA/A��Al�A ��A 5?@�dZ@�v�@�{@���@��/@�@���@�b@�@�h@��@��@�h@�b@�!@�z�@�bN@�O�@�@�7L@�Ĝ@��m@�Ĝ@�9X@�D@�@���@�^5@���@�V@��@�Z@�"�@��@�~�@�/@ݡ�@�r�@ܓu@�j@�l�@� �@�(�@�+@�v�@�r�@щ7@�$�@��T@ѡ�@мj@��m@�G�@���@̴9@̃@�Q�@�=q@���@���@���@őh@��/@�@��9@���@�@�bN@�@���@���@�~�@��7@��F@�+@��@�C�@�K�@�o@���@�z�@�1@�(�@���@�33@�^5@���@��@��j@��@�|�@�K�@�l�@��@��#@��-@��^@�J@���@�{@���@��@��P@��H@�n�@��@���@��7@�X@�?}@��@�j@�o@�E�@���@��7@�V@�bN@�7L@�O�@�%@�?}@�x�@��w@�\)@��;@�t�@��@���@��7@�X@�?}@��@�j@�o@�E�@���@��7@�V@�bN@�7L@�O�@�%@�?}@�x�@��w@�\)@��;@�t�@�r�@��^@��H@�ff@���@�@��\@��@��y@��R@�ȴ@��H@��H@���@��\@�^5@��@�Ĝ@�ƨ@�S�@�@��!@�ȴ@���@�=q@���@��/@��
@��\@�$�@��R@��
@���@���@�|�@���@�-@�p�@�1'@���@�+@��P@��
@�ƨ@�(�@�Q�@��w@�l�@���@��F@���@���@��;@��@��\@��^@��h@�O�@��@��@��9@�bN@��@���@��@���@��@���@��@�|�@�l�@�o@���@�^5@��#@�O�@�G�@�7L@�V@���@��9@��u@�bN@���@�S�@�33@�
=@���@�M�@��@���@���@�G�@���@�bN@�I�@�9X@�1@���@�C�@��@���@�^5@�5?@�@��@�@���@�E�@�~�@���@��@�o@�33@���@��R@�~�@�v�@�M�@�=q@�E�@�V@�^5@�^5@�^5@���@�/@�"h@x��@i��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B`BB`BB`BB_;BbNBaHB`BB`BB`BBhsB}�B��BJB{B�B�B�B�B�BuBDB
=B	7B1B1B1B1B1B	7B	7BPBJB�B�B�B�BhBPBJB
=B	7B	7B	7B	7B
=BVB{B�B�B!�B%�B"�B�B�B �B�BB�HB��B��B��BƨB��B�?B�B��B��B�\B�DB�1B�+B�B}�By�Bs�Bp�BW
B)�BDB��B�B�mB�
B��B�qB��B��B��B�{B�+B~�Bl�B_;BK�B>wB5?B/B'�B�BPB%B
��B
�B
�;B
��B
ÖB
�}B
�^B
�3B
��B
��B
�VB
�B
x�B
v�B
n�B
`BB
[#B
O�B
C�B
<jB
;dB
:^B
-B
+B
�B
oB
DB
  B	��B	�B	�mB	�BB	�B	��B	��B	��B	��B	��B	�FB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�=B	�B	u�B	m�B	cTB	[#B	M�B	B�B	5?B	/B	(�B	!�B	�B	{B	hB	1B	B	B��B��B��B��B�B�B�B�mB�5B�B�
B��B��B��BɺBƨBÖBB�}B�qB�dB�XB�?B�3B�B��B��B��B��B��B��B�oB�hB�VB�DB�7B�B�B|�Bx�Bs�Br�Bs�Bs�Bu�Bw�Bs�Bp�Bl�BgmBe`BbNB`BB]/B\)BYBVBR�BM�BJ�BI�BH�BG�BG�BE�BE�BE�BE�BC�BB�BA�BA�B?}B>wB>wB;dB:^B9XB8RB5?B6FB<jB8RB2-B/B/B/B-B-B0!B33B>wBM�BM�BL�BJ�BE�BJ�BS�BS�BW
BcTB{�B�B�%B�PB�VB�JB�DB�7B�Bz�B�Bw�Bt�B}�B�B�+B�1B�B|�B�B�B�%B�DB�JB�=B�1B�1B�+B�%B�7B�7B�7B�DB�DB�PB�hB�hB�bB�VB�=B�B}�B}�B�B�B�DB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�FB�LB�^B�dB�jB�}B��BĜBŢB��B��B��B��B��B��B�B�
B�B�B�B�/B�TB�`B�`B�`B�mB�B��B��B��B��B	B	B	B	%B	+B��B�B�
B�B�B�B�/B�TB�`B�`B�`B�mB�B��B��B��B��B	B	B	B	%B	DB	�B	 �B	�B	�B	'�B	7LB	<jB	@�B	D�B	H�B	K�B	N�B	O�B	Q�B	S�B	VB	YB	YB	\)B	`BB	aHB	ffB	iyB	jB	k�B	n�B	m�B	k�B	hsB	ffB	jB	q�B	x�B	x�B	x�B	x�B	v�B	w�B	t�B	s�B	t�B	x�B	|�B	~�B	�B	�%B	�+B	�1B	�PB	�\B	�oB	�oB	�oB	�uB	�oB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�9B	�LB	�^B	�^B	�dB	�qB	�wB	�wB	�}B	��B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�;B	�BB	�BB	�HB	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�WB	�B
?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B`BB`BB`BB_;BbNBaHB`BB`BB`BBhsB}�B��BJB{B�B�B�B�B�BuBDB
=B	7B1B1B1B1B1B	7B	7BPBJB�B�B�B�BhBPBJB
=B	7B	7B	7B	7B
=BVB{B�B�B!�B%�B"�B�B�B �B�BB�HB��B��B��BƨB��B�?B�B��B��B�\B�DB�1B�+B�B}�By�Bs�Bp�BW
B)�BDB��B�B�mB�
B��B�qB��B��B��B�{B�+B~�Bl�B_;BK�B>wB5?B/B'�B�BPB%B
��B
�B
�;B
��B
ÖB
�}B
�^B
�3B
��B
��B
�VB
�B
x�B
v�B
n�B
`BB
[#B
O�B
C�B
<jB
;dB
:^B
-B
+B
�B
oB
DB
  B	��B	�B	�mB	�BB	�B	��B	��B	��B	��B	��B	�FB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�=B	�B	u�B	m�B	cTB	[#B	M�B	B�B	5?B	/B	(�B	!�B	�B	{B	hB	1B	B	B��B��B��B��B�B�B�B�mB�5B�B�
B��B��B��BɺBƨBÖBB�}B�qB�dB�XB�?B�3B�B��B��B��B��B��B��B�oB�hB�VB�DB�7B�B�B|�Bx�Bs�Br�Bs�Bs�Bu�Bw�Bs�Bp�Bl�BgmBe`BbNB`BB]/B\)BYBVBR�BM�BJ�BI�BH�BG�BG�BE�BE�BE�BE�BC�BB�BA�BA�B?}B>wB>wB;dB:^B9XB8RB5?B6FB<jB8RB2-B/B/B/B-B-B0!B33B>wBM�BM�BL�BJ�BE�BJ�BS�BS�BW
BcTB{�B�B�%B�PB�VB�JB�DB�7B�Bz�B�Bw�Bt�B}�B�B�+B�1B�B|�B�B�B�%B�DB�JB�=B�1B�1B�+B�%B�7B�7B�7B�DB�DB�PB�hB�hB�bB�VB�=B�B}�B}�B�B�B�DB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�9B�FB�LB�^B�dB�jB�}B��BĜBŢB��B��B��B��B��B��B�B�
B�B�B�B�/B�TB�`B�`B�`B�mB�B��B��B��B��B	B	B	B	%B	+B��B�B�
B�B�B�B�/B�TB�`B�`B�`B�mB�B��B��B��B��B	B	B	B	%B	DB	�B	 �B	�B	�B	'�B	7LB	<jB	@�B	D�B	H�B	K�B	N�B	O�B	Q�B	S�B	VB	YB	YB	\)B	`BB	aHB	ffB	iyB	jB	k�B	n�B	m�B	k�B	hsB	ffB	jB	q�B	x�B	x�B	x�B	x�B	v�B	w�B	t�B	s�B	t�B	x�B	|�B	~�B	�B	�%B	�+B	�1B	�PB	�\B	�oB	�oB	�oB	�uB	�oB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�9B	�LB	�^B	�^B	�dB	�qB	�wB	�wB	�}B	��B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�;B	�BB	�BB	�HB	�`B	�`B	�fB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�WB	�B
?11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141540                              AO  ARCAADJP                                                                    20181024141540    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141540  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141540  QCF$                G�O�G�O�G�O�4000            