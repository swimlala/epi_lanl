CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:05Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @|   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       B@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  IL   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  a�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181024140805  20181024140805  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @פ����1   @פ�l�f@4C��$��c�V�u1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  BffB  B��B'��B0  B8  B@  BH  BP  BX  B_��Bh  Bp  Bx  B�  B�  B���B�  B�33B�33B�  B�  B�  B���B�  B�  C  C�fC	�fC  C  C  C  C�fC�fC�fC  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C��C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C��3C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3D   D � D ��D� D  Dy�D  D� D  D� D��Dy�D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*fD*� D+  D+� D,  D,� D-fD-� D-��D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4y�D5  D5� D6fD6�fD7fD7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D?��D@� DA  DAy�DB  DB� DC  DC�fDDfDD�fDE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DKfDK� DL  DLy�DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Do��Dp� Dq  Dq� Dr  Dr� Dr��Dsy�Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� DwٚDyw�D�G�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@\AG�A!G�AAG�AaG�A���A���A���A���A��
AУ�A��A��B Q�BQ�B�RBQ�B�B'�B0Q�B8Q�B@Q�BHQ�BPQ�BXQ�B_�BhQ�BpQ�BxQ�B�(�B�(�B���B�(�B�\)B�\)B�(�B�(�B�(�B���B�(�B�(�C{C��C	��C{C{C{C{C��C��C��C{C{C{C {C"{C${C&{C({C*{C+��C.{C0{C`{Cb{Cd{Cf{Cg��Cj{Cl{Cn{Cp{Cr{Ct{Cv.Cx{Cz{C|{C~{C�
C�
=C��pC�
=C�
=C�
=C�
C�
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
=C��pC��pC�
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
=C�
=C��pC�
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
=C��pC��pC�
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
=C��pC�
=C��pC��pC��pC��pC�
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
=C��pC�
=C�
C�
=C�
=C�
=C�
=C�
=C�
=C�
=C�
C�
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
=C��pD D �D ��D�DD~�DD�DD�D��D~�DD�DD�DD�D	D	�D
D
~�DD�DD�DD�D�D�DD�DD�DD�DD�DD�DD�DD�DD�D��D�DD�DD�D��D�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#~�D$D$�D%D%�D&D&�D'D'�D(D(��D)D)�D*�D*�D+D+�D,D,�D-�D-�D-��D.�D/D/�D0D0�D1D1��D2D2�D3D3�D4D4~�D5D5�D6�D6��D7�D7��D8D8�D9D9�D:D:�D;D;�D<D<��D=D=�D>D>�D?D?�D?��D@�DADA~�DBDB�DCDC��DD�DD��DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DK�DK�DLDL~�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DV��DW~�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�Dk�Dk�DlDl�DmDm�DnDn�DoDo~�Do��Dp�DqDq�DrDr�Dr��Ds~�DtDt�DuDu�DvDv�DwDw�Dw޹Dy|�D�J=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�&�A�(�A�&�A�"�A�/A�33A�33A�33A�5?A�5?A�7LA�7LA�9XA�9XA�9XA�;dA�;dA�=qA�=qA�?}A�?}A�A�A�C�A�C�A�A�A�A�A�A�A�C�A�C�A�E�A�C�A�E�A�G�A�K�A�C�A�;dA�9XA���A��;A��A���A�"�A��A�%A��A�1A��A��A���A�z�A�jA�9XA�%A��A��9A��PA�dZA�7LA���A���A�r�A��
A�p�A��HAs7LAp�HAm7LAhAa�hA^VA]XA\��A[33AYS�AXQ�AX-AW��AVĜAU�PAT�RAR��AQ%APbAOƨAO|�AO`BAO/AN=qAK`BAE�AB��A@�A?S�A=�A9|�A8�A8bNA6�jA3ƨA1�A.z�A+�#A)�A(E�A'oA%�;A%K�A$��A$n�A#�TA#��A"A!l�A;dAffA�mAbA�A�A=qA�DA9XAp�A�
A�hAC�A�A�A�A��A��A�AA�AJA?}Av�A1'A�
At�A
�`A
v�A
{A	�TA	ƨA	�FA	��A	��A	|�A�`A�RAM�A�^A+A�/AffA �A�A�A�+A�wAA�!A�RA~�An�AI�A9XA��A ��@�K�@��R@���@���@�-@��@�Q�@���@�o@�^5@�5?@�9X@�Q�@�j@�I�@�I�@�Z@�r�@�ƨ@�^@��
@��@�-@��@��@��@�S�@���@�  @�p�@܃@�C�@�/@�ff@���@��
@ӝ�@�;d@�@��y@�ȴ@�=q@�/@���@Ѓ@Ϯ@�V@�-@�{@���@��@�Q�@�ƨ@˕�@�\)@�
=@��H@ʧ�@�^5@��@ɩ�@ɉ7@�Z@��@�ff@�=q@�{@�@�?}@ģ�@�I�@���@�E�@�/@���@�1@�dZ@�"�@�@���@�ff@��@��#@���@��@�G�@��/@��@���@�I�@��@�t�@���@�A�@�S�@�@�ȴ@��R@�^5@��T@�X@��/@��9@��@��@��@���@�b@�33@�G�@���@�K�@���@�@��#@�E�@�V@��@�O�@���@��D@��D@�1'@��@��w@��@�l�@��\@�$�@��@�@�-@�n�@�-@���@��h@��7@��7@���@���@��@�O�@�%@��j@�j@�9X@���@�;d@�t�@�+@�33@�o@��H@��\@�-@�J@�@���@���@�bN@�C�@�ȴ@�5?@���@�A�@��F@�o@�^5@��@���@�X@��@�%@��9@�1'@���@�t�@�dZ@�C�@���@��#@��h@�`B@�/@��@���@��D@�A�@��@�C�@��@�
=@�@���@�5?@�@�{@���@��@���@��/@�r�@���@�+@���@�=q@���@��@��h@�p�@�`B@�O�@�/@��@���@���@���@�Q�@� �@�  @���@�33@��y@���@��R@��R@���@�n�@�V@�{@��h@�&�@��/@��j@��9@���@��u@�z�@�j@�bN@�Q�@��@�b@���@���@�;d@��y@���@���@�@��T@���@���@��@�hs@�X@�V@�z�@�bN@�Z@�Q�@�I�@��;@��@�\)@�33@�@��y@��H@��@�J@�%@��/@�I�@�1'@�b@��;@��F@���@���@�x@o��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�&�A�(�A�&�A�"�A�/A�33A�33A�33A�5?A�5?A�7LA�7LA�9XA�9XA�9XA�;dA�;dA�=qA�=qA�?}A�?}A�A�A�C�A�C�A�A�A�A�A�A�A�C�A�C�A�E�A�C�A�E�A�G�A�K�A�C�A�;dA�9XA���A��;A��A���A�"�A��A�%A��A�1A��A��A���A�z�A�jA�9XA�%A��A��9A��PA�dZA�7LA���A���A�r�A��
A�p�A��HAs7LAp�HAm7LAhAa�hA^VA]XA\��A[33AYS�AXQ�AX-AW��AVĜAU�PAT�RAR��AQ%APbAOƨAO|�AO`BAO/AN=qAK`BAE�AB��A@�A?S�A=�A9|�A8�A8bNA6�jA3ƨA1�A.z�A+�#A)�A(E�A'oA%�;A%K�A$��A$n�A#�TA#��A"A!l�A;dAffA�mAbA�A�A=qA�DA9XAp�A�
A�hAC�A�A�A�A��A��A�AA�AJA?}Av�A1'A�
At�A
�`A
v�A
{A	�TA	ƨA	�FA	��A	��A	|�A�`A�RAM�A�^A+A�/AffA �A�A�A�+A�wAA�!A�RA~�An�AI�A9XA��A ��@�K�@��R@���@���@�-@��@�Q�@���@�o@�^5@�5?@�9X@�Q�@�j@�I�@�I�@�Z@�r�@�ƨ@�^@��
@��@�-@��@��@��@�S�@���@�  @�p�@܃@�C�@�/@�ff@���@��
@ӝ�@�;d@�@��y@�ȴ@�=q@�/@���@Ѓ@Ϯ@�V@�-@�{@���@��@�Q�@�ƨ@˕�@�\)@�
=@��H@ʧ�@�^5@��@ɩ�@ɉ7@�Z@��@�ff@�=q@�{@�@�?}@ģ�@�I�@���@�E�@�/@���@�1@�dZ@�"�@�@���@�ff@��@��#@���@��@�G�@��/@��@���@�I�@��@�t�@���@�A�@�S�@�@�ȴ@��R@�^5@��T@�X@��/@��9@��@��@��@���@�b@�33@�G�@���@�K�@���@�@��#@�E�@�V@��@�O�@���@��D@��D@�1'@��@��w@��@�l�@��\@�$�@��@�@�-@�n�@�-@���@��h@��7@��7@���@���@��@�O�@�%@��j@�j@�9X@���@�;d@�t�@�+@�33@�o@��H@��\@�-@�J@�@���@���@�bN@�C�@�ȴ@�5?@���@�A�@��F@�o@�^5@��@���@�X@��@�%@��9@�1'@���@�t�@�dZ@�C�@���@��#@��h@�`B@�/@��@���@��D@�A�@��@�C�@��@�
=@�@���@�5?@�@�{@���@��@���@��/@�r�@���@�+@���@�=q@���@��@��h@�p�@�`B@�O�@�/@��@���@���@���@�Q�@� �@�  @���@�33@��y@���@��R@��R@���@�n�@�V@�{@��h@�&�@��/@��j@��9@���@��u@�z�@�j@�bN@�Q�@��@�b@���@���@�;d@��y@���@���@�@��T@���@���@��@�hs@�X@�V@�z�@�bN@�Z@�Q�@�I�@��;@��@�\)@�33@�@��y@��H@��@�J@�%@��/@�I�@�1'@�b@��;@��F@���@���@�x@o��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�ZB
�ZB
�`B
�`B
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�`B
�`B
�`B
�`B
�`B
�`B
�ZB
�ZB
�`B
�fB
�mB
�fB
�fB
�mB
�mB
�mB
�mB
�`B
�`B
�fB
�yB
�B
�B
�BB(�B<jBt�Bx�B_;B\)BR�BI�B@�B0!B$�B"�B!�B�B�B�B�B�B�BoBDB��B�B�`B�;B�B	�FB	��B	�B	[#B	5?B	�B	�B	hB	+B��B��B��B�B�B�`B�;B��B��BƨBĜBÖB��B�wB�RB��B��B�PB�+B�B{�Bt�Bs�Bq�Bm�BhsBdZBaHB_;B_;BbNBgmBjBiyBiyBhsBgmBe`BdZBaHBdZBbNBbNBbNBbNBaHBaHBbNBbNBdZBiyBjBiyBiyBk�Bn�Bn�Bx�B|�B}�B|�By�Bv�Bt�Bu�Bw�Bw�Bz�B}�B� B� B�B�B�B�B�=B�DB�PB�hB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�RB�XB�jB�}BÖBĜBĜBĜBǮBȴBȴB��B��B��B��B��B��B��B��B��B�B�)B�/B�/B�5B�;B�;B�HB�TB�mB�mB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	%B		7B	DB	JB	JB	VB	hB	uB	�B	�B	�B	�B	�B	�B	�B	#�B	+B	2-B	49B	9XB	?}B	D�B	H�B	K�B	N�B	S�B	W
B	YB	ZB	[#B	\)B	]/B	]/B	\)B	]/B	]/B	`BB	cTB	gmB	o�B	q�B	r�B	r�B	r�B	t�B	u�B	v�B	|�B	�B	�%B	�+B	�B	�B	�1B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�XB	�dB	�}B	��B	��B	��B	B	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�HB	�NB	�TB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
%B
%B
B
+B
1B
1B
	7B
	7B

=B

=B
DB
DB
DB
DB
DB

=B
JB
\B
\B
hB
hB
oB
oB
uB
uB
uB
aB
%�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�ZB
�ZB
�`B
�`B
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�`B
�`B
�`B
�`B
�`B
�`B
�ZB
�ZB
�`B
�fB
�mB
�fB
�fB
�mB
�mB
�mB
�mB
�`B
�`B
�fB
�yB
�B
�B
�BB(�B<jBt�Bx�B_;B\)BR�BI�B@�B0!B$�B"�B!�B�B�B�B�B�B�BoBDB��B�B�`B�;B�B	�FB	��B	�B	[#B	5?B	�B	�B	hB	+B��B��B��B�B�B�`B�;B��B��BƨBĜBÖB��B�wB�RB��B��B�PB�+B�B{�Bt�Bs�Bq�Bm�BhsBdZBaHB_;B_;BbNBgmBjBiyBiyBhsBgmBe`BdZBaHBdZBbNBbNBbNBbNBaHBaHBbNBbNBdZBiyBjBiyBiyBk�Bn�Bn�Bx�B|�B}�B|�By�Bv�Bt�Bu�Bw�Bw�Bz�B}�B� B� B�B�B�B�B�=B�DB�PB�hB�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�!B�-B�RB�XB�jB�}BÖBĜBĜBĜBǮBȴBȴB��B��B��B��B��B��B��B��B��B�B�)B�/B�/B�5B�;B�;B�HB�TB�mB�mB�yB�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	%B		7B	DB	JB	JB	VB	hB	uB	�B	�B	�B	�B	�B	�B	�B	#�B	+B	2-B	49B	9XB	?}B	D�B	H�B	K�B	N�B	S�B	W
B	YB	ZB	[#B	\)B	]/B	]/B	\)B	]/B	]/B	`BB	cTB	gmB	o�B	q�B	r�B	r�B	r�B	t�B	u�B	v�B	|�B	�B	�%B	�+B	�B	�B	�1B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�?B	�XB	�dB	�}B	��B	��B	��B	B	ĜB	ĜB	ŢB	ƨB	ƨB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�/B	�;B	�HB	�NB	�TB	�`B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
+B
%B
%B
B
+B
1B
1B
	7B
	7B

=B

=B
DB
DB
DB
DB
DB

=B
JB
\B
\B
hB
hB
oB
oB
uB
uB
uB
aB
%�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.08 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140805                              AO  ARCAADJP                                                                    20181024140805    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140805  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140805  QCF$                G�O�G�O�G�O�0               