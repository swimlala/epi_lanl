CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:08Z creation      
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
resolution        =���   axis      Z        l  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     l  L    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  Sl   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  e�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  mh   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     l  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181005190508  20181005190508  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @ץ�m#1   @ץ�""#�@3���+�c�����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C[�fC^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C��3C�  C��C��C��C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C��C�  C��3C�  C��C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D�fDfD� DfD� D��D� D  Dy�D��D� D��D	y�D	��D
y�D
��Dy�D  D� D  D�fD  Dy�D  D� D  D� D  Dy�D  D� DfD� D��Dy�D��D� D  D� D��Dy�D  D� D��Dy�D��D� D  Dy�D��D� D  D�fDfD�fD  D� D   D y�D ��D!y�D"  D"� D#fD#�fD$fD$�fD%fD%� D&  D&� D&��D'y�D3� D4  D4y�D5  D5�fD6fD6� D7  D7� D8fD8�fD9fD9� D:  D:�fD;  D;� D<  D<� D=  D=� D>  D>� D?fD?� D@  D@� DA  DA� DB  DB�fDCfDC�fDD  DD�fDE  DEy�DF  DF� DG  DG� DH  DHy�DI  DI� DJ  DJ� DKfDK� DL  DL� DM  DM�fDN  DNy�DO  DO�fDP  DP� DQfDQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDafDa� Db  Db� Dc  Dc� DdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dly�Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Ds� Dt  Dt� Du  Du� Dv  Dv�fDwfDw� Dw� Dy�)D�=�D�޸1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��G@�z�A=qA"=qAB=qAb=qA��A��A��A��A��A��A��A��B �\B�\B�\B�\B �\B(�\B0�\B8�\B@(�BH�\BP�\BX�\B`�\Bh�\Bp�\Bx�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�C #�C#�C#�C#�C#�C
#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C#�C #�C"#�C$#�C&#�C(#�C*#�C,#�C.#�C0#�C2#�C4#�C6#�C8#�C:#�C<#�C>#�C@#�CB#�CD#�CF#�CH#�CJ#�CL#�CN#�CP#�CR#�CT#�CV#�CX#�CZ#�C\
=C^#�C`#�Cb#�Cd#�Cf#�Ch#�Cj#�Cl#�Cn#�Cp#�Cr#�Ct#�Cv#�Cx#�Cz#�C|#�C~#�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C�C��C��C�C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C�C�C��C��C��C�C��C��C��C�C��C��C��C��C��C��C��C��C�C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D�\D\D��D\D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D�\D�D��D�D��D�D��D�D��D�D��D\D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�\D\D�\D�D��D �D ��D!�D!��D"�D"��D#\D#�\D$\D$�\D%\D%��D&�D&��D'�D'��D3��D4�D4��D5�D5�\D6\D6��D7�D7��D8\D8�\D9\D9��D:�D:�\D;�D;��D<�D<��D=�D=��D>�D>��D?\D?��D@�D@��DA�DA��DB�DB�\DC\DC�\DD�DD�\DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK\DK��DL�DL��DM�DM�\DN�DN��DO�DO�\DP�DP��DQ\DQ��DR�DR��DS�DS��DT�DT�\DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`�\Da\Da��Db�Db��Dc�Dc��Dd\Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr�\Ds�Ds��Dt�Dt��Du�Du��Dv�Dv�\Dw\Dw��Dw��Dy�D�B>D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A̾wA���A�A�A�ĜA�ȴA�ȴA�ȴA�ƨA�ĜA�A̼jA̴9A̬A̬A̴9A̸RA�A�ƨA���A̍PA�=qA�+A� �A� �A�"�A� �A��A�{A�%A���A���A���A��A�
=AˑhA��mAʟ�A�p�A�z�A�XA�JA���Aɕ�A��A���A��A�S�AǬAǝ�A�jA���A�&�A�  A�ȴAś�A�jA�bA��yAĺ^A�O�A�oA���AÁA�E�A��A�|�A��`A�Q�A�7LA���A�?}A���A���A�+A�&�A�ĜA�v�A��#A�A���A��A���A��HA�x�A��/A���A�r�A�A�$�A�-A��
A�A���A�dZA�Q�A��FA���A�A��#A��A�ZA���A��A��
A�1A�ZA��A��A�1'A��PA�1'A�bA�{A��/A��A� �A�t�A���A�~�A���A�oA���A�#A}�PAz�DAy�mAu�Am�FAkC�Ac��AbȴAa&�A`�DA`  A]�;AX�AU��AR�uANȴAL�AJ�`AH��AHE�AG|�AD�yACl�AB�9AA��A>jA=dZA;�-A8�/A5�7A3`BA/�hA-��A-�A-XA+%A'x�A%XA$�\A$E�A"I�A   A^5A�AC�A�At�A~�A�A�yAffA��A�/A�A��A�AĜA�AZAC�A�AVA~�A�AAC�A=qA/Az�A$�AdZA
�HA
�!A	�-AZAx�Az�A^5A�Ax�Av�A+AJA �y@��@�5?@�G�@�I�@��;@�t�@�M�@��9@���@�@�Q�@�t�@��y@�n�@�J@�@�Ĝ@���@�@�hs@�j@�Z@��@���@�1@�33@�-@��@�Ĝ@� �@�;d@���@܋D@���@�?}@���@֗�@ԣ�@҇+@��/@У�@�9X@�;d@��T@�X@��@���@�Z@��@�-@���@ɉ7@�G�@��@�r�@��@Ǿw@�|�@�@Ɵ�@Ł@�z�@���@�
=@�M�@�7L@��@��@��m@���@�S�@�+@�"�@��@��R@�n�@�=q@�-@��@�O�@��@�bN@���@�@��@�&�@���@��u@���@���@�v�@�ff@�^5@�V@�V@�M�@�M�@�5?@�@��^@��h@�hs@�?}@��@�%@���@���@���@��@�A�@�b@��w@�(�@�x�@���@���@�@���@��7@���@�dZ@���@�o@���@�X@�(�@�hs@��@��9@�9X@��w@��P@�dZ@�S�@�C�@���@���@�^5@�=q@���@��-@���@�x�@�`B@�O�@�O�@�O�@�G�@�G�@�?}@�?}@�G�@�G�@�`B@��7@���@��@��@��#@���@�@��h@�V@���@�r�@�bN@�Z@�I�@�1@��m@�ƨ@���@�t�@�33@��H@��+@�n�@�n�@�E�@�{@��@��T@��h@�hs@�/@��@��@��@�%@��@�I�@�l�@�K�@���@�V@���@�hs@��`@��@�r�@�A�@�(�@�b@���@��w@�l�@��H@���@�V@��@�hs@��@���@�bN@�Q�@�1@�ƨ@�;d@��y@�ȴ@���@�V@�E�@�E�@�=q@�{@���@���@�hs@��@���@�A�@���@���@���@���@��P@�dZ@�33@�o@�@��R@�ff@�E�@��@���@��#@���@�O�@��@���@��@�(�@�ƨ@�l�@�dZ@�dZ@�dZ@�dZ@�dZ@�l�@�l�@�S�@�
=@�[�@~� @i��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A̾wA���A�A�A�ĜA�ȴA�ȴA�ȴA�ƨA�ĜA�A̼jA̴9A̬A̬A̴9A̸RA�A�ƨA���A̍PA�=qA�+A� �A� �A�"�A� �A��A�{A�%A���A���A���A��A�
=AˑhA��mAʟ�A�p�A�z�A�XA�JA���Aɕ�A��A���A��A�S�AǬAǝ�A�jA���A�&�A�  A�ȴAś�A�jA�bA��yAĺ^A�O�A�oA���AÁA�E�A��A�|�A��`A�Q�A�7LA���A�?}A���A���A�+A�&�A�ĜA�v�A��#A�A���A��A���A��HA�x�A��/A���A�r�A�A�$�A�-A��
A�A���A�dZA�Q�A��FA���A�A��#A��A�ZA���A��A��
A�1A�ZA��A��A�1'A��PA�1'A�bA�{A��/A��A� �A�t�A���A�~�A���A�oA���A�#A}�PAz�DAy�mAu�Am�FAkC�Ac��AbȴAa&�A`�DA`  A]�;AX�AU��AR�uANȴAL�AJ�`AH��AHE�AG|�AD�yACl�AB�9AA��A>jA=dZA;�-A8�/A5�7A3`BA/�hA-��A-�A-XA+%A'x�A%XA$�\A$E�A"I�A   A^5A�AC�A�At�A~�A�A�yAffA��A�/A�A��A�AĜA�AZAC�A�AVA~�A�AAC�A=qA/Az�A$�AdZA
�HA
�!A	�-AZAx�Az�A^5A�Ax�Av�A+AJA �y@��@�5?@�G�@�I�@��;@�t�@�M�@��9@���@�@�Q�@�t�@��y@�n�@�J@�@�Ĝ@���@�@�hs@�j@�Z@��@���@�1@�33@�-@��@�Ĝ@� �@�;d@���@܋D@���@�?}@���@֗�@ԣ�@҇+@��/@У�@�9X@�;d@��T@�X@��@���@�Z@��@�-@���@ɉ7@�G�@��@�r�@��@Ǿw@�|�@�@Ɵ�@Ł@�z�@���@�
=@�M�@�7L@��@��@��m@���@�S�@�+@�"�@��@��R@�n�@�=q@�-@��@�O�@��@�bN@���@�@��@�&�@���@��u@���@���@�v�@�ff@�^5@�V@�V@�M�@�M�@�5?@�@��^@��h@�hs@�?}@��@�%@���@���@���@��@�A�@�b@��w@�(�@�x�@���@���@�@���@��7@���@�dZ@���@�o@���@�X@�(�@�hs@��@��9@�9X@��w@��P@�dZ@�S�@�C�@���@���@�^5@�=q@���@��-@���@�x�@�`B@�O�@�O�@�O�@�G�@�G�@�?}@�?}@�G�@�G�@�`B@��7@���@��@��@��#@���@�@��h@�V@���@�r�@�bN@�Z@�I�@�1@��m@�ƨ@���@�t�@�33@��H@��+@�n�@�n�@�E�@�{@��@��T@��h@�hs@�/@��@��@��@�%@��@�I�@�l�@�K�@���@�V@���@�hs@��`@��@�r�@�A�@�(�@�b@���@��w@�l�@��H@���@�V@��@�hs@��@���@�bN@�Q�@�1@�ƨ@�;d@��y@�ȴ@���@�V@�E�@�E�@�=q@�{@���@���@�hs@��@���@�A�@���@���@���@���@��P@�dZ@�33@�o@�@��R@�ff@�E�@��@���@��#@���@�O�@��@���@��@�(�@�ƨ@�l�@�dZ@�dZ@�dZ@�dZ@�dZ@�l�@�l�@�S�@�
=@�[�@~� @i��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
\B
bB
bB
bB
bB
hB
{B
�B
�B
�B
�B
{B
uB
uB
�B
�B
�B
+B
1'B
:^B
I�B
O�B
O�B
R�B
XB
[#B
\)B
\)B
]/B
^5B
aHB
e`B
l�B
� B
��B
�B
�B
��B
�B
�5B
�mB
�B
��BB
��B
��B
��BuB.B_;Bo�B� B� B�{B��B�?B�^BĜB��B��B�B��BB
=B\B�B/BH�BF�BL�BXBYBYBXBW
BXBYBXBVBT�BO�BG�BE�BB�B<jB7LB5?B0!B'�B �B\BDBJB��B�
BŢB�RB��B`BBR�B7LB&�B�BuBVBBB
��B
�B
�ZB
�/B
�#B
�B
��B
�jB
��B
�oB
z�B
cTB
P�B
H�B
8RB
5?B
$�B
oB	��B	�B	��B	��B	�hB	s�B	l�B	iyB	ffB	bNB	S�B	5?B	#�B	�B	
=B	B��B��B�B�B�yB�`B�NB�5B�B��BȴB��BĜB��B��BŢB��BȴB�}B�B��B��B��B�PB�B~�B~�B� B|�B|�B�B�JB�\B�\B�bB�hB�hB�hB�oB�oB�oB�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�-B�-B�'B�!B�!B�B�B�B�B�!B�!B�'B�-B�-B�'B�9B�?B�LB�dB��BĜBƨBƨB��B��B��B��B��B��B�
B�)B�5B�BB�HB�NB�TB�fB�yB�yB�B�B�B�B�B�B��B��B��B	B	B	B	+B	1B	1B		7B	
=B	DB	DB	DB	DB	JB	PB	VB	\B	oB	�B	�B	�B	�B	$�B	.B	1'B	33B	49B	5?B	5?B	5?B	6FB	6FB	8RB	:^B	;dB	<jB	=qB	>wB	>wB	>wB	?}B	?}B	C�B	G�B	G�B	I�B	Q�B	]/B	ffB	iyB	l�B	k�B	m�B	� B	�%B	�7B	�DB	�PB	�PB	K�B	�?B	�RB	�XB	�^B	�^B	�dB	�dB	�dB	�jB	�}B	ÖB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�5B	�5B	�5B	�5B	�5B	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�mB	�sB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
	7B

=B
DB
JB
PB
PB
PB
PB
PB
PB
PB
JB
JB
VB
pB
 vB
.�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222244222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B
\B
bB
bB
bB
bB
hB
{B
�B
�B
�B
�B
{B
uB
uB
�B
�B
�B
+B
1'B
:^B
I�B
O�B
O�B
R�B
XB
[#B
\)B
\)B
]/B
^5B
aHB
e`B
l�B
� B
��B
�B
�B
��B
�B
�5B
�mB
�B
��BB
��B
��B
��BuB.B_;Bo�B� B� B�{B��B�?B�^BĜB��B��B�B��BB
=B\B�B/BH�BF�BL�BXBYBYBXBW
BXBYBXBVBT�BO�BG�BE�BB�B<jB7LB5?B0!B'�B �B\BDBJB��B�
BŢB�RB��B`BBR�B7LB&�B�BuBVBBB
��B
�B
�ZB
�/B
�#B
�B
��B
�jB
��B
�oB
z�B
cTB
P�B
H�B
8RB
5?B
$�B
oB	��B	�B	��B	��B	�hB	s�B	l�B	iyB	ffB	bNB	S�B	5?B	#�B	�B	
=B	B��B��B�B�B�yB�`B�NB�5B�B��BȴB��BĜB��B��BŢB��BȴB�}B�B��B��B��B�PB�B~�B~�B� B|�B|�B�B�JB�\B�\B�bB�hB�hB�hB�oB�oB�oB�{B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�'B�-B�-B�'B�!B�!B�B�B�B�B�!B�!B�'B�-B�-B�'B�9B�?B�LB�dB��BĜBƨBƨB��B��B��B��B��B��B�
B�)B�5B�BB�HB�NB�TB�fB�yB�yB�B�B�B�B�B�B��B��B��B	B	B	B	+B	1B	1B		7B	
=B	DB	DB	DB	DB	JB	PB	VB	\B	oB	�B	�B	�B	�B	$�B	.B	1'B	33B	49B	5?B	5?B	5?B	6FB	6FB	8RB	:^B	;dB	<jB	=qB	>wB	>wB	>wB	?}B	?}B	C�B	G�B	G�B	I�B	Q�B	]/B	ffB	iyB	l�B	k�B	m�B	� B	�%B	�7B	�DB	�PB	�PB	K�B	�?B	�RB	�XB	�^B	�^B	�dB	�dB	�dB	�jB	�}B	ÖB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�5B	�5B	�5B	�5B	�5B	�BB	�BB	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�`B	�mB	�sB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
  B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
1B
	7B

=B
DB
JB
PB
PB
PB
PB
PB
PB
PB
JB
JB
VB
pB
 vB
.�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222244222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.14 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190508                              AO  ARCAADJP                                                                    20181005190508    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190508  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190508  QCF$                G�O�G�O�G�O�C000            