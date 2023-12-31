CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:33Z creation      
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
resolution        =���   axis      Z           9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ^    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        `   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   h(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        j0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        rP   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   zp   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        |x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141533  20181024141533  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @��$���1   @��%[�@8*��n��d%$�/1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �B   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,�fD-fD-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT� DU  DU� DV  DVy�DW  DW� DX  DX� DY  DY� DY��DZy�DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_y�D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Duy�Dv  Dv� Dw  Dw� Dw�3Dy�fD�M�D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111@��\@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A�p�A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B���B���B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C��C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C$.C&{C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
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
=C�
C�
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
=D D �DD�DD�DD~�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%��D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,��D-�D-��D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D>��D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS~�DTDT�DUDU�DVDV~�DWDW�DXDX�DYDY�DY��DZ~�DZ��D[�D\D\�D]D]�D^D^�D_D_~�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu~�DvDv�DwDw�Dw�RDy��D�PRD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�v�A�v�A�t�A�p�A�t�A�r�A�ffA�;dA�ȴA�bA�dZA�S�A�I�A�5?A�33A�7LA�33A�?}A�=qA�=qA�=qA�7LA�1'A�/A�7LA�A�A�=qA�33A�&�A��A�{A�VA�oA� �A�"�A��A�$�A�&�A�$�A�$�A�$�A�"�A� �A��A��A�A���AƃA��A� �A� �A��\A�r�A�5?A�+A�A�z�A��A���A�+A���A��A�I�A��A��A���A�VA���A�9XA�v�A��A���A�ĜA�~�A�"�A�O�A���A�ZA���A�-A�bNA���A�A�A��A�JA�hsA��A�M�A�VA��/A�=qA��A�bNA��HA�A�1'A�x�A�"�A��PA��^A��DA��A�ffA�A�A���A�VA�x�A���A���A��+A��A���A�K�A���A� �A���A�$�A���A���A�I�A��A�A���A��wA���A��-A�+A�v�A��/A���A�A}�A|VAz�9Ax^5Av��As33Ao�Al�`Ak?}Ail�Ah$�Ag�AeG�Aax�A^z�A\I�A[��AZ��AY�FAY�AX��AX��AW�AV��AVffAU�TATĜATbNASO�AP��AOK�ANȴAM�AMAKl�AJ^5AIt�AH�AG�
AFQ�AE
=ADr�AC/A?��A:��A:VA9��A9�#A9��A9��A9t�A9+A7��A6^5A4��A4VA3S�A2r�A1�FA09XA.�A.I�A.E�A.=qA.5?A.bA,bNA)��A(�A(�A'33A&��A&-A$  A!��A �jA�HA9XAp�AQ�A`BA�/A^5A�PA��A��A��Ar�A�AK�A�
A�A��A�A$�A�A�^Al�A
��A
�A
5?A	��A�/Ap�AbNAbA�A�A�^A�A��AdZA33A ��A v�@���@��+@�5?@��T@���@���@�;d@�`B@���@���@�I�@�l�@�u@���@��@�o@���@�p�@�C�@�O�@�  @��y@���@���@�dZ@���@��`@�;d@��T@��@��/@�z�@� �@��
@׍P@�E�@�b@җ�@�$�@�9X@��@�ff@�hs@��@˝�@���@�J@��T@���@�x�@ȋD@Ǿw@�K�@ƸR@�ff@�-@��@�v�@�"�@�+@�"�@�33@�@Ɨ�@�=q@�-@�~�@�ff@��@���@�@��T@�V@�/@��@���@��@�A�@�33@�$�@���@��`@�bN@�9X@�ƨ@�l�@�\)@�J@��h@�X@�/@���@��@�A�@���@�Z@��@�  @��;@��F@��@�E�@���@�&�@��9@��;@��@��@��@�M�@�&�@��u@�I�@� �@��w@�C�@���@���@���@���@���@��@��;@�33@��R@�-@��-@�X@���@�l�@�o@���@���@���@���@��@���@���@��\@�E�@�`B@��@���@��/@���@��m@��@���@��@��@��@�S�@��H@��y@��y@��y@��y@��H@���@��\@�v�@�~�@��!@���@��P@��@���@�v�@�ff@�$�@�S�@���@��D@��9@���@� �@��m@��@���@��H@�v�@�$�@��h@�Ĝ@��j@�A�@�(�@� �@� �@�b@�b@�9X@��@���@�"�@���@�ff@�^5@���@�{@�p�@���@��@��@��/@�A�@���@��@���@�\)@��R@�v�@���@���@��R@�o@�+@��@���@�\)@�\)@���@��H@�33@�o@��@���@��9@��@�J@��@�V@��@�I�@��@���@��R@�o@�+@��@���@�\)@�\)@���@��H@�33@�o@��@���@��9@��@�J@��@�V@��@�I�@��@���@�@�E�@��@��^@�x�@�7L@�&�@���@�Ĝ@�1'@��;@�ƨ@�ƨ@���@s�m@]S&1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111A�v�A�v�A�t�A�p�A�t�A�r�A�ffA�;dA�ȴA�bA�dZA�S�A�I�A�5?A�33A�7LA�33A�?}A�=qA�=qA�=qA�7LA�1'A�/A�7LA�A�A�=qA�33A�&�A��A�{A�VA�oA� �A�"�A��A�$�A�&�A�$�A�$�A�$�A�"�A� �A��A��A�A���AƃA��A� �A� �A��\A�r�A�5?A�+A�A�z�A��A���A�+A���A��A�I�A��A��A���A�VA���A�9XA�v�A��A���A�ĜA�~�A�"�A�O�A���A�ZA���A�-A�bNA���A�A�A��A�JA�hsA��A�M�A�VA��/A�=qA��A�bNA��HA�A�1'A�x�A�"�A��PA��^A��DA��A�ffA�A�A���A�VA�x�A���A���A��+A��A���A�K�A���A� �A���A�$�A���A���A�I�A��A�A���A��wA���A��-A�+A�v�A��/A���A�A}�A|VAz�9Ax^5Av��As33Ao�Al�`Ak?}Ail�Ah$�Ag�AeG�Aax�A^z�A\I�A[��AZ��AY�FAY�AX��AX��AW�AV��AVffAU�TATĜATbNASO�AP��AOK�ANȴAM�AMAKl�AJ^5AIt�AH�AG�
AFQ�AE
=ADr�AC/A?��A:��A:VA9��A9�#A9��A9��A9t�A9+A7��A6^5A4��A4VA3S�A2r�A1�FA09XA.�A.I�A.E�A.=qA.5?A.bA,bNA)��A(�A(�A'33A&��A&-A$  A!��A �jA�HA9XAp�AQ�A`BA�/A^5A�PA��A��A��Ar�A�AK�A�
A�A��A�A$�A�A�^Al�A
��A
�A
5?A	��A�/Ap�AbNAbA�A�A�^A�A��AdZA33A ��A v�@���@��+@�5?@��T@���@���@�;d@�`B@���@���@�I�@�l�@�u@���@��@�o@���@�p�@�C�@�O�@�  @��y@���@���@�dZ@���@��`@�;d@��T@��@��/@�z�@� �@��
@׍P@�E�@�b@җ�@�$�@�9X@��@�ff@�hs@��@˝�@���@�J@��T@���@�x�@ȋD@Ǿw@�K�@ƸR@�ff@�-@��@�v�@�"�@�+@�"�@�33@�@Ɨ�@�=q@�-@�~�@�ff@��@���@�@��T@�V@�/@��@���@��@�A�@�33@�$�@���@��`@�bN@�9X@�ƨ@�l�@�\)@�J@��h@�X@�/@���@��@�A�@���@�Z@��@�  @��;@��F@��@�E�@���@�&�@��9@��;@��@��@��@�M�@�&�@��u@�I�@� �@��w@�C�@���@���@���@���@���@��@��;@�33@��R@�-@��-@�X@���@�l�@�o@���@���@���@���@��@���@���@��\@�E�@�`B@��@���@��/@���@��m@��@���@��@��@��@�S�@��H@��y@��y@��y@��y@��H@���@��\@�v�@�~�@��!@���@��P@��@���@�v�@�ff@�$�@�S�@���@��D@��9@���@� �@��m@��@���@��H@�v�@�$�@��h@�Ĝ@��j@�A�@�(�@� �@� �@�b@�b@�9X@��@���@�"�@���@�ff@�^5@���@�{@�p�@���@��@��@��/@�A�@���@��@���@�\)@��R@�v�@���@���@��R@�o@�+@��@���@�\)@�\)@���@��H@�33@�o@��@���@��9@��@�J@��@�V@��@�I�@��@���@��R@�o@�+@��@���@�\)@�\)@���@��H@�33@�o@��@���@��9@��@�J@��@�V@��@�I�@��@���@�@�E�@��@��^@�x�@�7L@�&�@���@�Ĝ@�1'@��;@�ƨ@�ƨ@���@s�m@]S&1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B$�B9XBT�B`BB^5B^5B\)B\)BbNBbNBffBffBgmBgmBffBffBffBhsBm�Bm�Bl�BiyBhsBgmBffBgmBjBjBjBk�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bl�Bl�Bl�Bk�BaHBr�B�+B�{B��B�?B�RB�jB��B��BÖBŢBĜB��B�wB�^B�dB�'B��B��B��B��B��B��B��B��B��B��B�bB�=Bz�Br�Bl�BiyBcTB\)BVBP�BF�B7LB'�B�BuBB�B�mB�/B��B��B�dB�B��B��B��B��B��B��B��B��B�oB�bB�VB�B}�Br�Be`BW
BI�BA�B1'B+B(�B$�B�BJB
��B
�B
�mB
�5B
��B
�B
�JB
y�B
ffB
^5B
H�B
7LB
"�B
%B	��B	�B	�`B	�#B	��B	�}B	��B	�B	r�B	k�B	iyB	dZB	^5B	^5B	_;B	[#B	Q�B	N�B	O�B	L�B	J�B	E�B	9XB	/B	+B	$�B	�B	{B	JB	%B��B��B�B�mB�TB�#B��B�B��B��B��B��B��B��B��B��B��B�hB�bB�bB�VB�VB�VB�hB�bB�bB�\B�\B�PB�oB�oB�uB�oB�uB�uB�{B�hB�B{�Bv�Bp�Bk�Bl�Bm�Bm�Bm�Bl�BhsBdZBaHB`BB^5B^5BZBW
BP�BN�BL�BL�BK�BK�BI�BG�BF�BE�BA�B9XB2-B1'B/B-B+B'�B'�B&�B&�B&�B%�B%�B#�B$�B#�B#�B"�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B �B�B �B!�B �B �B �B!�B#�B#�B"�B%�B%�B%�B'�B(�B(�B+B+B+B)�B+B.B0!B2-B5?B9XBD�B]/B_;BffBhsBiyBjBl�Bm�Bt�B� B�DB�uB��B��B�{B�=B�7B�DB�JB�DB�=B�7B�%B�B�B�+B�+B�1B�7B�DB�\B�\B�VB�\B�hB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B�B�B�!B�9B�LB�RB�qB��B��BĜBƨBɺB��B��B��B��B�B�B�B�B�HB�fB�B�B�B�B�B��B��B��B��B		7B	\B	bB	hB	oB	uB	�B	�B	 �B	"�B	#�B	&�B	0!B	9XB	<jB	<jB	=qB	=qB	>wB	F�B	J�B	M�B	N�B	Q�B	T�B	ZB	aHB	s�B	}�B	�B	�B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�9B	�3B	�-B	�-B	�'B	�'B	�9B	�LB	�RB	�LB	�LB	�dB	�jB	�XB	�9B	�B	��B	��B	��B	��B	�B	�!B	�3B	�?B	�XB	�jB	��B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	ɺB	ǮB	ĜB	ĜB	ŢB	ǮB	ɺB	�!B	�3B	�?B	�XB	�jB	��B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	ɺB	ǮB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	ּB	�9B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111B�B�B�B�B�B�B�B$�B9XBT�B`BB^5B^5B\)B\)BbNBbNBffBffBgmBgmBffBffBffBhsBm�Bm�Bl�BiyBhsBgmBffBgmBjBjBjBk�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bl�Bl�Bl�Bk�BaHBr�B�+B�{B��B�?B�RB�jB��B��BÖBŢBĜB��B�wB�^B�dB�'B��B��B��B��B��B��B��B��B��B��B�bB�=Bz�Br�Bl�BiyBcTB\)BVBP�BF�B7LB'�B�BuBB�B�mB�/B��B��B�dB�B��B��B��B��B��B��B��B��B�oB�bB�VB�B}�Br�Be`BW
BI�BA�B1'B+B(�B$�B�BJB
��B
�B
�mB
�5B
��B
�B
�JB
y�B
ffB
^5B
H�B
7LB
"�B
%B	��B	�B	�`B	�#B	��B	�}B	��B	�B	r�B	k�B	iyB	dZB	^5B	^5B	_;B	[#B	Q�B	N�B	O�B	L�B	J�B	E�B	9XB	/B	+B	$�B	�B	{B	JB	%B��B��B�B�mB�TB�#B��B�B��B��B��B��B��B��B��B��B��B�hB�bB�bB�VB�VB�VB�hB�bB�bB�\B�\B�PB�oB�oB�uB�oB�uB�uB�{B�hB�B{�Bv�Bp�Bk�Bl�Bm�Bm�Bm�Bl�BhsBdZBaHB`BB^5B^5BZBW
BP�BN�BL�BL�BK�BK�BI�BG�BF�BE�BA�B9XB2-B1'B/B-B+B'�B'�B&�B&�B&�B%�B%�B#�B$�B#�B#�B"�B!�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B �B!�B!�B �B�B �B!�B �B �B �B!�B#�B#�B"�B%�B%�B%�B'�B(�B(�B+B+B+B)�B+B.B0!B2-B5?B9XBD�B]/B_;BffBhsBiyBjBl�Bm�Bt�B� B�DB�uB��B��B�{B�=B�7B�DB�JB�DB�=B�7B�%B�B�B�+B�+B�1B�7B�DB�\B�\B�VB�\B�hB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B��B��B�B�B�!B�9B�LB�RB�qB��B��BĜBƨBɺB��B��B��B��B�B�B�B�B�HB�fB�B�B�B�B�B��B��B��B��B		7B	\B	bB	hB	oB	uB	�B	�B	 �B	"�B	#�B	&�B	0!B	9XB	<jB	<jB	=qB	=qB	>wB	F�B	J�B	M�B	N�B	Q�B	T�B	ZB	aHB	s�B	}�B	�B	�B	�PB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�-B	�9B	�3B	�-B	�-B	�'B	�'B	�9B	�LB	�RB	�LB	�LB	�dB	�jB	�XB	�9B	�B	��B	��B	��B	��B	�B	�!B	�3B	�?B	�XB	�jB	��B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	ɺB	ǮB	ĜB	ĜB	ŢB	ǮB	ɺB	�!B	�3B	�?B	�XB	�jB	��B	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	ɺB	ǮB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	ּB	�9B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141411111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141533                              AO  ARCAADJP                                                                    20181024141533    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141533  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141533  QCF$                G�O�G�O�G�O�4000            