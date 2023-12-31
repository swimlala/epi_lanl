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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181024141533  20181024141533  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @��d���1   @��e/h^2@81&�x���d#-1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �B   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'fD'� D(  D(� D(��D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DwٚDy�3D�:�D�Ϯ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��\@\AG�A!G�AAG�AaG�A���A���A���A���A���AУ�A��A��B Q�BQ�BQ�BQ�B Q�B(Q�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B`Q�BhQ�BpQ�BxQ�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�\)B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�B�(�C {C��C{C{C{C
{C{C{C{C{C{C{C{C{C{C{C {C"{C${C%��C({C*{C,{C.{C0{C2{C4{C6{C8{C:{C<{C>{C@{CB{CD{CF{CH{CJ{CL{CN{CP{CR{CT{CV{CX{CZ{C\{C^{C`{Cb{Cd{Cf{Ch{Cj{Cl{Cn{Cp{Cr{Ct{Cv{Cx{Cz{C|{C~{C�
C�
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
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
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
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#��D$D$�D%D%�D&D&�D'�D'�D(D(�D(��D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�DuDu�DvDv�DwDw�Dw޹Dy�RD�=pD��=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A�$�A�$�A�$�A�&�A�+A�+A�+A��A�%A�  A��Aɺ^Aɲ-AɋDA�n�A�`BA�K�A�A��;AȁA�%AǏ\A�r�A�ffA�n�Aǡ�AǇ+A�|�A�z�A�v�A�l�A�^5A�S�A�?}A�7LA�5?A��A��
A�5?A�ȴA���A�hsA��HA��yA��A�1'A�bA�?}A��A��\A�1'A��A� �A���A�A��uA��A�ĜA�
=A��/A�/A���A�%A���A���A���A���A��#A��A��A�&�A��^A��PA�
=A�S�A��DA�?}A�ĜA�  A��FA��/A�;dA�l�A�"�A��A�ffA�1'A��A��+A�dZA�;dA�1A���A�Q�A�1A�A���A��RA�ZA�`BA��/A�
=A��TA�A�A��A��wA��uA�`BA��HA���A�~�A�r�A��hA�K�A���A�5?A��A�~�A�z�A��A�l�A��uA���A�/A��yA�G�A�#A�A}�TA{�Av��AtQ�Ar��Aq�PAodZAmt�AkS�AhJAgC�Ag%Af�Af��Ae|�AcO�A`��A^�A]A\ȴA[C�AZȴAZ�uAYt�AW/AUXAT=qAR$�AQ�AQhsAP�HAP1AO\)AN�HAN1AL�`AKAJ5?AH�/AF�yAD�yAC�AB��A@ZA=
=A<9XA<JA;��A;�A;�mA;��A;��A:�A7�A6��A6r�A4�+A4I�A2jA0�`A0�uA0v�A.n�A,�A,ZA+`BA*�+A)�7A(ffA'�A&JA$jA"�A!��A!oA �!A v�AƨAt�AS�A+A�A��A�;A�A��A �A�TA1'A?}A�/Az�A(�AAXA �A�7A��AA�A�uA�9A�^A\)AA
M�A	�PA	K�A	+A��AbA��A�A�#AE�AhsA ��A ~�A ^5A ^5A M�@��F@���@���@���@�~�@�r�@�@�~�@�@�Q�@�G�@�\)@���@�\@��@�@�x�@��;@��@޸R@�O�@�j@� �@�o@ڇ+@�p�@�r�@�p�@��@Гu@�+@θR@���@�hs@�hs@�X@�`B@�Q�@ˮ@�ȴ@ɑh@�?}@� �@Ƨ�@š�@�M�@�K�@ǶF@�r�@�p�@��@Гu@�+@θR@���@�hs@�hs@�X@�`B@�Q�@ˮ@�ȴ@ɑh@�?}@� �@Ƨ�@š�@�M�@�K�@ǶF@Ǯ@Ǯ@ǥ�@ǅ@�K�@�
=@���@��H@�"�@��@��@�Ĝ@�j@��y@���@��@��@��#@��@��/@��`@��u@�|�@��@�ȴ@�p�@�%@���@�33@�^5@�~�@���@���@��/@���@���@��j@��u@��w@�5?@��T@��^@��h@�X@���@�bN@��!@��@��m@���@�$�@�@�p�@��9@�bN@�1'@��m@�dZ@�33@���@�=q@�X@�bN@�dZ@�{@���@��^@��h@��@�X@�&�@�G�@�x�@��u@��@�S�@��@�+@�33@��@��!@���@���@�E�@���@��7@�7L@���@��D@�1'@��@���@��P@��P@�K�@�33@�+@�"�@��@�o@���@���@��R@���@��\@���@�o@�;d@���@���@�Ĝ@�=q@��@���@��7@�~�@�+@��w@��;@��F@��H@�V@�=q@�J@�p�@��7@��h@��h@���@�\)@���@��+@�ff@�-@�5?@�{@�@��h@�hs@��@��m@�-@���@�z�@��@���@��P@��@�$�@�$�@�n�@�1@�O�@�p�@�x�@�x�@�x�@�x�@���@�K�@��@���@�hs@�`B@��^@�M�@�ff@��+@�ȴ@�G�@�S�@�~�@�^5@�-@�@���@�G�@���@�1'@���@�33@��R@�~�@��@��-@���@�p�@�hs@�X@�G�@��/@�z�@�A�@�  @�  @��;@v��@`�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��A�$�A�$�A�$�A�&�A�+A�+A�+A��A�%A�  A��Aɺ^Aɲ-AɋDA�n�A�`BA�K�A�A��;AȁA�%AǏ\A�r�A�ffA�n�Aǡ�AǇ+A�|�A�z�A�v�A�l�A�^5A�S�A�?}A�7LA�5?A��A��
A�5?A�ȴA���A�hsA��HA��yA��A�1'A�bA�?}A��A��\A�1'A��A� �A���A�A��uA��A�ĜA�
=A��/A�/A���A�%A���A���A���A���A��#A��A��A�&�A��^A��PA�
=A�S�A��DA�?}A�ĜA�  A��FA��/A�;dA�l�A�"�A��A�ffA�1'A��A��+A�dZA�;dA�1A���A�Q�A�1A�A���A��RA�ZA�`BA��/A�
=A��TA�A�A��A��wA��uA�`BA��HA���A�~�A�r�A��hA�K�A���A�5?A��A�~�A�z�A��A�l�A��uA���A�/A��yA�G�A�#A�A}�TA{�Av��AtQ�Ar��Aq�PAodZAmt�AkS�AhJAgC�Ag%Af�Af��Ae|�AcO�A`��A^�A]A\ȴA[C�AZȴAZ�uAYt�AW/AUXAT=qAR$�AQ�AQhsAP�HAP1AO\)AN�HAN1AL�`AKAJ5?AH�/AF�yAD�yAC�AB��A@ZA=
=A<9XA<JA;��A;�A;�mA;��A;��A:�A7�A6��A6r�A4�+A4I�A2jA0�`A0�uA0v�A.n�A,�A,ZA+`BA*�+A)�7A(ffA'�A&JA$jA"�A!��A!oA �!A v�AƨAt�AS�A+A�A��A�;A�A��A �A�TA1'A?}A�/Az�A(�AAXA �A�7A��AA�A�uA�9A�^A\)AA
M�A	�PA	K�A	+A��AbA��A�A�#AE�AhsA ��A ~�A ^5A ^5A M�@��F@���@���@���@�~�@�r�@�@�~�@�@�Q�@�G�@�\)@���@�\@��@�@�x�@��;@��@޸R@�O�@�j@� �@�o@ڇ+@�p�@�r�@�p�@��@Гu@�+@θR@���@�hs@�hs@�X@�`B@�Q�@ˮ@�ȴ@ɑh@�?}@� �@Ƨ�@š�@�M�@�K�@ǶF@�r�@�p�@��@Гu@�+@θR@���@�hs@�hs@�X@�`B@�Q�@ˮ@�ȴ@ɑh@�?}@� �@Ƨ�@š�@�M�@�K�@ǶF@Ǯ@Ǯ@ǥ�@ǅ@�K�@�
=@���@��H@�"�@��@��@�Ĝ@�j@��y@���@��@��@��#@��@��/@��`@��u@�|�@��@�ȴ@�p�@�%@���@�33@�^5@�~�@���@���@��/@���@���@��j@��u@��w@�5?@��T@��^@��h@�X@���@�bN@��!@��@��m@���@�$�@�@�p�@��9@�bN@�1'@��m@�dZ@�33@���@�=q@�X@�bN@�dZ@�{@���@��^@��h@��@�X@�&�@�G�@�x�@��u@��@�S�@��@�+@�33@��@��!@���@���@�E�@���@��7@�7L@���@��D@�1'@��@���@��P@��P@�K�@�33@�+@�"�@��@�o@���@���@��R@���@��\@���@�o@�;d@���@���@�Ĝ@�=q@��@���@��7@�~�@�+@��w@��;@��F@��H@�V@�=q@�J@�p�@��7@��h@��h@���@�\)@���@��+@�ff@�-@�5?@�{@�@��h@�hs@��@��m@�-@���@�z�@��@���@��P@��@�$�@�$�@�n�@�1@�O�@�p�@�x�@�x�@�x�@�x�@���@�K�@��@���@�hs@�`B@��^@�M�@�ff@��+@�ȴ@�G�@�S�@�~�@�^5@�-@�@���@�G�@���@�1'@���@�33@��R@�~�@��@��-@���@�p�@�hs@�X@�G�@��/@�z�@�A�@�  @�  @��;@v��@`�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B"�B%�B&�B-B2-B2-B49B5?B5?B6FB9XB8RB>wBD�BF�BE�BG�BN�B`BBaHBcTBgmBhsBiyBjBk�Bk�Bk�Bk�Bl�Bn�BffBffBo�B|�Bx�By�B�B�B�B�+B�bB��B��B��B��B��B�9B�dB�}B��B��B�B�)B�HB�ZBŢB�B�B��B�B��BƨB�XB�FB�FB��Bq�BgmBbNBbNB\)BZBQ�BL�BB�B<jB,B{B��B�B�B�B�B�B�mB�HB��B�^B�!B�B�B��B��B�{B~�Bp�BcTBZBW
BR�BI�BK�B7LB+B�BoB1B
��B
��B
�sB
�#B
��B
ŢB
�jB
�-B
��B
�oB
�JB
�B
}�B
o�B
aHB
:^B
�B
uB
DB
B	��B	�B	�)B	�B	��B	��B	��B	ĜB	�-B	��B	�VB	�B	}�B	q�B	k�B	hsB	bNB	Q�B	D�B	=qB	2-B	6FB	49B	5?B	0!B	+B	%�B	�B	�B	uB	VB	B��B�B�ZB�#B��B�9B�B��B��B��B��B��B��B��B�uB�hB�hB�VB�JB�PB�7B�+B�%B�JB�=B�=B�PB�PB�JB�VB�oB�\B�JB�+B|�By�Bv�Bs�Bt�Br�Br�Bq�Bq�Bp�Bp�Bn�Bn�Bl�BffBdZBbNBcTBbNBaHB_;B^5B\)BZBYBXBVBO�BL�BJ�BI�BI�BG�BE�BD�BC�B@�B>wB;dB8RB/B)�B(�B'�B'�B&�B%�B%�B%�B"�B"�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B$�B$�B%�B%�B%�B'�B'�B'�B'�B&�B+B.B6FB6FB49B6FB7LBB�BW
BcTBhsB�B$�B$�B%�B%�B%�B'�B'�B'�B'�B&�B+B.B6FB6FB49B6FB7LBB�BW
BcTBhsBjBjBk�Bk�Bl�Bn�Bs�Bx�B�1B��B��B��B��B��B��B��B��B�uB�hB�hB�uB�{B�hB�\B�PB�DB�JB�\B�\B�VB�\B�oB�{B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�'B�9B�FB�LB�XB�qB�wB�wBÖBɺB��B��B��B��B��B��B�B�B�B�;B�ZB�sB�B�B�B�B�B��B��B��B	B	
=B	JB	\B	hB	oB	{B	�B	�B	�B	 �B	!�B	2-B	5?B	6FB	7LB	9XB	:^B	=qB	F�B	J�B	J�B	J�B	L�B	P�B	^5B	\)B	hsB	t�B	� B	�+B	�+B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�FB	�LB	�LB	�LB	�LB	�LB	�9B	�!B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�wB	�}B	�}B	�}B	�}B	�}B	�}B	�jB	�^B	�dB	�jB	�wB	��B	ǮB	��B	��B	��B	��B	ȴB	ŢB	ƨB	ƨB	ǮB	ȴB	ǮB	ȴB	ȴB	ǮB	ȴB	ȴB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	՛B	�B
M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B"�B%�B&�B-B2-B2-B49B5?B5?B6FB9XB8RB>wBD�BF�BE�BG�BN�B`BBaHBcTBgmBhsBiyBjBk�Bk�Bk�Bk�Bl�Bn�BffBffBo�B|�Bx�By�B�B�B�B�+B�bB��B��B��B��B��B�9B�dB�}B��B��B�B�)B�HB�ZBŢB�B�B��B�B��BƨB�XB�FB�FB��Bq�BgmBbNBbNB\)BZBQ�BL�BB�B<jB,B{B��B�B�B�B�B�B�mB�HB��B�^B�!B�B�B��B��B�{B~�Bp�BcTBZBW
BR�BI�BK�B7LB+B�BoB1B
��B
��B
�sB
�#B
��B
ŢB
�jB
�-B
��B
�oB
�JB
�B
}�B
o�B
aHB
:^B
�B
uB
DB
B	��B	�B	�)B	�B	��B	��B	��B	ĜB	�-B	��B	�VB	�B	}�B	q�B	k�B	hsB	bNB	Q�B	D�B	=qB	2-B	6FB	49B	5?B	0!B	+B	%�B	�B	�B	uB	VB	B��B�B�ZB�#B��B�9B�B��B��B��B��B��B��B��B�uB�hB�hB�VB�JB�PB�7B�+B�%B�JB�=B�=B�PB�PB�JB�VB�oB�\B�JB�+B|�By�Bv�Bs�Bt�Br�Br�Bq�Bq�Bp�Bp�Bn�Bn�Bl�BffBdZBbNBcTBbNBaHB_;B^5B\)BZBYBXBVBO�BL�BJ�BI�BI�BG�BE�BD�BC�B@�B>wB;dB8RB/B)�B(�B'�B'�B&�B%�B%�B%�B"�B"�B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B$�B$�B%�B%�B%�B'�B'�B'�B'�B&�B+B.B6FB6FB49B6FB7LBB�BW
BcTBhsB�B$�B$�B%�B%�B%�B'�B'�B'�B'�B&�B+B.B6FB6FB49B6FB7LBB�BW
BcTBhsBjBjBk�Bk�Bl�Bn�Bs�Bx�B�1B��B��B��B��B��B��B��B��B�uB�hB�hB�uB�{B�hB�\B�PB�DB�JB�\B�\B�VB�\B�oB�{B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�'B�9B�FB�LB�XB�qB�wB�wBÖBɺB��B��B��B��B��B��B�B�B�B�;B�ZB�sB�B�B�B�B�B��B��B��B	B	
=B	JB	\B	hB	oB	{B	�B	�B	�B	 �B	!�B	2-B	5?B	6FB	7LB	9XB	:^B	=qB	F�B	J�B	J�B	J�B	L�B	P�B	^5B	\)B	hsB	t�B	� B	�+B	�+B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�FB	�LB	�LB	�LB	�LB	�LB	�9B	�!B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�wB	�}B	�}B	�}B	�}B	�}B	�}B	�jB	�^B	�dB	�jB	�wB	��B	ǮB	��B	��B	��B	��B	ȴB	ŢB	ƨB	ƨB	ǮB	ȴB	ǮB	ȴB	ȴB	ǮB	ȴB	ȴB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	՛B	�B
M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141533                              AO  ARCAADJP                                                                    20181024141533    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141533  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141533  QCF$                G�O�G�O�G�O�4000            