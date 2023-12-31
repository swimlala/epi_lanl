CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:47Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190547  20181005190547  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��i�z��1   @��j%�Y@1���v��c�1&�y1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�ff@���@���A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�33B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�fC  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Co�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C��3C��3C�  C�  C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C�  C��3C��3C�  C�  C��3C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D ��D� DfD� D  D�fD  Dy�D��D� D  D� D  D� D��D� D	  D	y�D
  D
� D  D� D  D� D  D� D��D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D� D  Dy�D  D�fDfD� D  D�fD  D� D  D� D  Dy�D��Dy�D��D � D!fD!�fD"  D"� D#  D#� D#��D$� D$��D%� D&  D&� D'  D'� D(fD(�fD)  D)� D*  D*� D*��D+� D,  D,� D-  D-�fD.fD.� D/  D/y�D/��D0y�D0��D1� D2  D2y�D3  D3� D4  D4� D5  D5�fD6  D6� D7  D7�fD8  D8� D9  D9� D:  D:�fD;  D;� D<  D<� D=fD=� D>  D>� D>��D?� D@fD@� D@��DA� DBfDB� DC  DC� DC��DD� DE  DE� DF  DF�fDG  DGy�DH  DH� DIfDI� DJ  DJ�fDK  DKy�DL  DL� DM  DM� DM��DN� DO  DO� DO��DPy�DQ  DQ�fDR  DR� DS  DS� DT  DT� DUfDU� DV  DVy�DV��DW� DX  DX� DYfDY� DY��DZ� D[fD[� D\  D\� D]  D]� D^  D^�fD_fD_� D`  D`� DafDa�fDb  Db�fDc  Dc�fDdfDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dj��Dk� Dk��Dly�Dl��Dmy�Dm��Dny�Do  Do� Dp  Dp�fDq  Dq� Dr  Dry�Ds  Ds� Ds��Dt� DufDu� Dv  Dv� Dw  Dwy�DwٚDy��D�=D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�Q�A (�A!AAAaA��HA��HA��HA��HA��A��HA��HA�{B p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bxp�B�k�B�8RB�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�8RB�k�B�k�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C�C)C)C )C")C$)C&)C()C*)C,)C.�C0)C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX5�CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj)Cl)Cn)Cp�Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�GC�GC�GC�C�C�C�C�C��C��C��C�C�C�C�GC�GC�C�C�C�GC�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C��C��C�C�GC�GC�C�C�GC�C�C��C�C�GC�C�C�C�C�C�GC�GC�C�C�C�C�C��C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
D ��D �D�
DpD�
D
D�pD
D��D �D�
D
D�
D
D�
D �D�
D	
D	��D

D
�
D
D�
D
D�
D
D�
D �D�
DpD�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D��D �D�
D
D��D
D�pDpD�
D
D�pD
D�
D
D�
D
D��D �D��D  �D �
D!pD!�pD"
D"�
D#
D#�
D$ �D$�
D% �D%�
D&
D&�
D'
D'�
D(pD(�pD)
D)�
D*
D*�
D+ �D+�
D,
D,�
D-
D-�pD.pD.�
D/
D/��D0 �D0��D1 �D1�
D2
D2��D3
D3�
D4
D4�
D5
D5�pD6
D6�
D7
D7�pD8
D8�
D9
D9�
D:
D:�pD;
D;�
D<
D<�
D=pD=�
D>
D>�
D? �D?�
D@pD@�
DA �DA�
DBpDB�
DC
DC�
DD �DD�
DE
DE�
DF
DF�pDG
DG��DH
DH�
DIpDI�
DJ
DJ�pDK
DK��DL
DL�
DM
DM�
DN �DN�
DO
DO�
DP �DP��DQ
DQ�pDR
DR�
DS
DS�
DT
DT�
DUpDU�
DV
DV��DW �DW�
DX
DX�
DYpDY�
DZ �DZ�
D[pD[�
D\
D\�
D]
D]�
D^
D^�pD_pD_�
D`
D`�
DapDa�pDb
Db�pDc
Dc�pDdpDd�
De
De�
Df
Df�
Dg
Dg�
Dh
Dh�
Di
Di�
Dj
Dj�
Dk �Dk�
Dl �Dl��Dm �Dm��Dn �Dn��Do
Do�
Dp
Dp�pDq
Dq�
Dr
Dr��Ds
Ds�
Dt �Dt�
DupDu�
Dv
Dv�
Dw
Dw��Dw�Dy��D�@�D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�/A�9XA�E�A�C�A�;dA�;dA�?}A�?}A�?}A�?}A�5?A�7LA�;dA�;dA�=qA�E�A�K�A�VA�I�A�VA�$�A�hsA˃A�ƨA�/A�O�A�ZA�S�A�C�A�9XA�+A��A��mA˴9A˺^A˩�Aˉ7A˩�A�
=A���A�ĜA�Q�A�VAʁA�-A�  Aɩ�A�/AȬA�x�A�&�A��A�  A�7LA�I�A��!A�p�A�5?A��9A�ĜA���A��A�;dA�$�A��#A�9XA���A�?}A��FA��\A�O�A��yA���A��TA���A���A�Q�A���A�O�A���A�G�A� �A��A��HA���A�p�A���A��A� �A�
=A���A��A��A���A�I�A���A�bA�/A�jA��+A���A�7LA��A��TA}��A{%Ax�AvJAq��AmƨAh1'Ab  A`z�A^�A\��A[K�AWdZAVQ�AU�ATJAQ�PAO��AN�ALz�AJ�jAI�AF��AD�DAC��AB1'A@v�A?\)A=O�A933A6bNA4M�A0�HA/��A.�yA.^5A-��A,��A,{A+�A(��A$��A${A"�\A!�wA!��A!|�A!�Ap�A�!AXA�A��A1A|�A&�A�!A�A�uA�^A&�A��A�A�hAv�A1A�^A;dA�`A��A��A�RA�Av�AQ�AhsAr�A�TA��AoA
�AĜAZAVA�9A�-A��A ��@��@���@�Z@�n�@��@�Z@�|�@�E�@���@�G�@��j@�G�@�/@��@�V@���@��#@�`B@�1'@�33@�^5@�@�j@�I�@��y@�@��/@�b@⟾@ᙚ@�@�p�@�7L@��u@ߝ�@�~�@��T@�&�@�Ĝ@۝�@�5?@�7L@ؓu@٩�@�X@���@�V@�A�@�(�@��@�\)@�33@�"�@�ȴ@�@�X@�  @�o@Ώ\@θR@��@��@��/@�j@��;@��@�ff@���@ɉ7@���@�r�@ǶF@��@��H@���@�E�@ź^@ŉ7@�p�@�x�@�7L@ļj@ģ�@ă@�A�@å�@î@�t�@��H@\@�-@���@��@�p�@�X@��`@��u@�Z@��@���@���@��@�^5@�@�7L@�V@�%@���@���@�r�@��
@�K�@�K�@�S�@�K�@�C�@�+@�C�@�;d@�+@��@��@�o@���@�=q@�p�@��@�1'@�dZ@�;d@��y@��\@�=q@���@���@�9X@���@�ȴ@��\@�M�@���@���@���@��h@��h@�`B@���@�(�@�K�@���@���@��@��@���@��@� �@��;@��w@���@�|�@�;d@��H@���@�~�@�ff@�M�@�5?@��@��#@��h@�O�@�7L@�/@��`@��j@���@��D@��@�I�@�1@�  @���@��
@�ƨ@�S�@��@�
=@��H@���@�~�@�^5@�=q@��@��T@���@�O�@�G�@�V@�1'@��m@�l�@��@�~�@�5?@�{@��@�{@��@�@��7@��/@�z�@�Z@�I�@��@�\)@��@���@�$�@���@�?}@��@��`@�bN@�9X@�(�@��@��@���@�"�@��H@���@���@�ff@��@��@�X@�O�@�?}@���@��`@���@�9X@��
@��@���@�dZ@�S�@�"�@�o@��@�ȴ@�v�@���@�`B@�G�@�?}@��@��`@���@��D@�Q�@���@�"�@��@��y@���@���@��@��T@���@��-@�p�@�G�@��@��j@�j@�1'@��
@�dZ@�K�@�o@��@��R@���@���@�~�@�E�@���@��@��`@���@�I�@�A�@��@���@�dZ@���@���@��+@�^5@���@�@��4@��\@g9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�/A�9XA�E�A�C�A�;dA�;dA�?}A�?}A�?}A�?}A�5?A�7LA�;dA�;dA�=qA�E�A�K�A�VA�I�A�VA�$�A�hsA˃A�ƨA�/A�O�A�ZA�S�A�C�A�9XA�+A��A��mA˴9A˺^A˩�Aˉ7A˩�A�
=A���A�ĜA�Q�A�VAʁA�-A�  Aɩ�A�/AȬA�x�A�&�A��A�  A�7LA�I�A��!A�p�A�5?A��9A�ĜA���A��A�;dA�$�A��#A�9XA���A�?}A��FA��\A�O�A��yA���A��TA���A���A�Q�A���A�O�A���A�G�A� �A��A��HA���A�p�A���A��A� �A�
=A���A��A��A���A�I�A���A�bA�/A�jA��+A���A�7LA��A��TA}��A{%Ax�AvJAq��AmƨAh1'Ab  A`z�A^�A\��A[K�AWdZAVQ�AU�ATJAQ�PAO��AN�ALz�AJ�jAI�AF��AD�DAC��AB1'A@v�A?\)A=O�A933A6bNA4M�A0�HA/��A.�yA.^5A-��A,��A,{A+�A(��A$��A${A"�\A!�wA!��A!|�A!�Ap�A�!AXA�A��A1A|�A&�A�!A�A�uA�^A&�A��A�A�hAv�A1A�^A;dA�`A��A��A�RA�Av�AQ�AhsAr�A�TA��AoA
�AĜAZAVA�9A�-A��A ��@��@���@�Z@�n�@��@�Z@�|�@�E�@���@�G�@��j@�G�@�/@��@�V@���@��#@�`B@�1'@�33@�^5@�@�j@�I�@��y@�@��/@�b@⟾@ᙚ@�@�p�@�7L@��u@ߝ�@�~�@��T@�&�@�Ĝ@۝�@�5?@�7L@ؓu@٩�@�X@���@�V@�A�@�(�@��@�\)@�33@�"�@�ȴ@�@�X@�  @�o@Ώ\@θR@��@��@��/@�j@��;@��@�ff@���@ɉ7@���@�r�@ǶF@��@��H@���@�E�@ź^@ŉ7@�p�@�x�@�7L@ļj@ģ�@ă@�A�@å�@î@�t�@��H@\@�-@���@��@�p�@�X@��`@��u@�Z@��@���@���@��@�^5@�@�7L@�V@�%@���@���@�r�@��
@�K�@�K�@�S�@�K�@�C�@�+@�C�@�;d@�+@��@��@�o@���@�=q@�p�@��@�1'@�dZ@�;d@��y@��\@�=q@���@���@�9X@���@�ȴ@��\@�M�@���@���@���@��h@��h@�`B@���@�(�@�K�@���@���@��@��@���@��@� �@��;@��w@���@�|�@�;d@��H@���@�~�@�ff@�M�@�5?@��@��#@��h@�O�@�7L@�/@��`@��j@���@��D@��@�I�@�1@�  @���@��
@�ƨ@�S�@��@�
=@��H@���@�~�@�^5@�=q@��@��T@���@�O�@�G�@�V@�1'@��m@�l�@��@�~�@�5?@�{@��@�{@��@�@��7@��/@�z�@�Z@�I�@��@�\)@��@���@�$�@���@�?}@��@��`@�bN@�9X@�(�@��@��@���@�"�@��H@���@���@�ff@��@��@�X@�O�@�?}@���@��`@���@�9X@��
@��@���@�dZ@�S�@�"�@�o@��@�ȴ@�v�@���@�`B@�G�@�?}@��@��`@���@��D@�Q�@���@�"�@��@��y@���@���@��@��T@���@��-@�p�@�G�@��@��j@�j@�1'@��
@�dZ@�K�@�o@��@��R@���@���@�~�@�E�@���@��@��`@���@�I�@�A�@��@���@�dZ@���@���@��+@�^5@���@�@��4@��\@g9�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B��B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	D�B	q�B	�oB	�RB	��B
�B
C�B
XB
cTB
n�B
�B
��B
�oB
��B
��B
�fB
�BB%�B33BJ�B\)B_;BhsBm�Bp�B}�B�B��B��B��B�B�^BÖB��B�B'�B5?B8RBB�BB�BK�BQ�BP�BK�BE�BE�BC�BB�BH�BD�B?}B:^B9XB7LB33B$�B�BoBB�B�HB��B��B�^B��B�bBy�B[#B(�B
��B
�yB
�HB
��B
�^B
��B
x�B
jB
Q�B
2-B
uB	��B	�HB	��B	�wB	�B	��B	�{B	r�B	R�B	49B	'�B	�B	uB		7B��B�B�mB�`B�5B�
B��B��BŢB�wB�qB�dB�XB�FB�3B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�-B�-B�-B�-B�'B�'B�9B�FB�FB�RB�FB�LB�?B�B�-B�LB�dB�dB�}B�}B�qB�}B�}B�}B��BŢBɺB��B��B��B�B�BB�/B�B�
B�
B�B�
B�B�#B�/B�BB�BB�TB�`B�fB�mB�B�B�B�B�B�B�B�B�B�B��B��B	B	%B	1B	uB	�B	oB	hB	oB	uB	{B	�B	{B	�B	�B	�B	�B	�B	 �B	"�B	$�B	&�B	+B	,B	/B	33B	:^B	=qB	>wB	?}B	?}B	?}B	C�B	H�B	I�B	J�B	I�B	H�B	I�B	K�B	P�B	R�B	R�B	XB	YB	ZB	YB	ZB	]/B	]/B	]/B	^5B	`BB	dZB	e`B	ffB	jB	n�B	p�B	r�B	v�B	y�B	z�B	z�B	x�B	w�B	x�B	x�B	z�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�1B	�7B	�7B	�=B	�=B	�DB	�JB	�PB	�PB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�3B	�3B	�3B	�9B	�?B	�FB	�FB	�LB	�LB	�RB	�RB	�XB	�dB	�jB	�qB	�qB	�}B	��B	B	B	B	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B

=B
�B
$&B
2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  B�B�B��B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B	D�B	q�B	�oB	�RB	��B
�B
C�B
XB
cTB
n�B
�B
��B
�oB
��B
��B
�fB
�BB%�B33BJ�B\)B_;BhsBm�Bp�B}�B�B��B��B��B�B�^BÖB��B�B'�B5?B8RBB�BB�BK�BQ�BP�BK�BE�BE�BC�BB�BH�BD�B?}B:^B9XB7LB33B$�B�BoBB�B�HB��B��B�^B��B�bBy�B[#B(�B
��B
�yB
�HB
��B
�^B
��B
x�B
jB
Q�B
2-B
uB	��B	�HB	��B	�wB	�B	��B	�{B	r�B	R�B	49B	'�B	�B	uB		7B��B�B�mB�`B�5B�
B��B��BŢB�wB�qB�dB�XB�FB�3B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�-B�-B�-B�-B�-B�'B�'B�9B�FB�FB�RB�FB�LB�?B�B�-B�LB�dB�dB�}B�}B�qB�}B�}B�}B��BŢBɺB��B��B��B�B�BB�/B�B�
B�
B�B�
B�B�#B�/B�BB�BB�TB�`B�fB�mB�B�B�B�B�B�B�B�B�B�B��B��B	B	%B	1B	uB	�B	oB	hB	oB	uB	{B	�B	{B	�B	�B	�B	�B	�B	 �B	"�B	$�B	&�B	+B	,B	/B	33B	:^B	=qB	>wB	?}B	?}B	?}B	C�B	H�B	I�B	J�B	I�B	H�B	I�B	K�B	P�B	R�B	R�B	XB	YB	ZB	YB	ZB	]/B	]/B	]/B	^5B	`BB	dZB	e`B	ffB	jB	n�B	p�B	r�B	v�B	y�B	z�B	z�B	x�B	w�B	x�B	x�B	z�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�1B	�7B	�7B	�=B	�=B	�DB	�JB	�PB	�PB	�VB	�\B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�3B	�3B	�3B	�3B	�9B	�?B	�FB	�FB	�LB	�LB	�RB	�RB	�XB	�dB	�jB	�qB	�qB	�}B	��B	B	B	B	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�mB	�mB	�mB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
  B
  B
B
B
B
B
B
B
B
B
%B
%B
+B
1B
1B
1B
	7B

=B
�B
$&B
2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190547                              AO  ARCAADJP                                                                    20181005190547    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190547  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190547  QCF$                G�O�G�O�G�O�8000            