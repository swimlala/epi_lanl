CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:06:12Z creation      
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
resolution        =���   axis      Z        p  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J,   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  Sx   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Z�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  \�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  d4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  f   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  m�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  t�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  v�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~<   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     p  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �4   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �D   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �H   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �`   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �dArgo profile    3.1 1.2 19500101000000  20181005190612  20181005190612  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL              &A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��Ŭ�1   @��-��@1������c�bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                     &A   A   B   @9��@�  @�  A   A   A@  A`  A�  A���A���A�  A�  A�  A�33A�  B ffB  B  BffB ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C�fC  C  C	�fC  C�C  C  C�C  C�C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN�CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C{�fC~  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fDfD�fD  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D��Dy�D��Dy�D��Dy�D  Dy�D��D� D  D�fD  Dy�D  D� D  D�fDfD� D��D� D  D� DfD�fDfD� D   D � D!fD!� D"fD"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*�fD+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4� D4��D5� D6  D6� D7  D7y�D8  D8� D9  D9� D9��D:y�D;  D;� D<  D<� D=  D=� D=��D>y�D>��D?� D?��D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DF��DG� DH  DT  DT� DUfDU�fDVfDV� DWfDW�fDX  DXy�DY  DY� DZ  DZ�fD[  D[� D\  D\�fD]  D]� D^  D^�fD_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� DffDf� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dry�Dr��Dsy�Dt  Dt� Du  Du� Dv  Dv� DwfDw� Dw��DyuD�G\D�Ϯ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@K�@���@���Az�A$z�ADz�Adz�A�=qA�
>A�
>A�=qA�=qA�=qA�p�A�=qB�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B�B��\B��\B��\B��\B��\B��\B�\)B��\B��\B��\B��\B��\B��\B��\B��\Bď\B�B�BЏ\Bԏ\B؏\B܏\B��\B�\B�\)B�\B��\B�\B��\B��\C G�CG�C.CG�CG�C
.CG�CaHCG�CG�CaHCG�CaHCG�CG�CG�C G�C"G�C$G�C&G�C(G�C*.C,G�C.G�C0G�C2G�C4G�C6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNaHCPG�CRG�CTG�CVG�CXG�CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�ClG�CnG�CpG�CrG�CtG�CvG�CxG�CzG�C|.C~G�C�#�C�#�C�
C�
C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�
C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�D �D ��D�D�RDRD�RD�D��D�D��D�D��D�D��D�D��D�D��D	RD	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�RD�D��D�D��D�D�RDRD��D�D��D�D��DRD�RDRD��D �D ��D!RD!��D"RD"��D#�D#�RD$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*�RD+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4RD4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DT�DT��DURDU�RDVRDV��DWRDW�RDX�DX��DY�DY��DZ�DZ�RD[�D[��D\�D\�RD]�D]��D^�D^�RD_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��DfRDf��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp�RDq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��DwRDw��Dw޹Dy�D�PRD�ؤ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�^5A�bNA�bNA�bNA�ZA�^5A�bNA�dZA�dZA�dZA�dZA�jA�l�A�n�A�n�A�p�A�p�A�p�A�n�A�l�A�l�A�n�A�n�A�p�A�p�A�p�A�n�A�l�A�n�A�t�A�t�A�r�A�bNA�C�A�bNA�A�ȴAʼjA��A��Aȉ7A��A�-A�r�A�bAŸRA�A�Q�A��mA���A��A�$�A��+A�bNA�K�A�;dA�(�A���A���A��DA�9XA�O�A��FA� �A�33A��A�1A��\A�|�A�JA�jA�(�A�A��9A��7A��A��A�ĜA��+A���A��FA�x�A�z�A�K�A�VA��A�r�A�A�5?A�Q�A��PA���A���A��
A�+A��PA�?}A�r�A��mA��A�A���A�S�A�9XA�^5A��A���A��A~�`Az��Au\)ArAl(�Ai�^Ag�TAfAd �A`ȴA\�RAZ��AW��AU�AUS�AS�TAQ/AO�AN�AM�#AL�/AL$�AJȴAF��AD��AB(�A?��A>��A;�wA9�TA8�yA7ƨA6��A5�hA4�A4=qA2�A/�A,��A,5?A+�^A*�A)VA(�A(ZA'�A'C�A&��A%&�A#�TA ��A jA�mA�/AoA�yA��AȴA��AS�A��A�A�RA�+A�A�A"�A��A�A�;A��A�uA33A��A1'A�\A�-A��A33A�A��AVA�A;dA
ZA	/A~�AȴA(�AA9XAJAA`BAoAĜA$�A  A�A�mA��AG�A%A �/A �DA {@�@��-@�Q�@�^5@��;@�/@�-@홚@�`B@�G�@�V@�`B@�+@�9X@�ƨ@���@�J@�+@�$�@���@�dZ@�\@�  @�l�@�u@�Q�@�F@���@�O�@�J@�!@旍@�\@旍@��@�7L@�&�@�Ĝ@��m@�v�@�Ĝ@� �@��;@ߕ�@���@��@�ȴ@���@݉7@�G�@�V@�j@���@�(�@ۮ@�\)@�C�@�
=@�E�@�G�@���@ؼj@�Z@�ƨ@׮@�dZ@��y@֗�@�5?@ա�@�/@��`@Դ9@�Q�@Ӿw@�t�@�S�@���@�@�%@�r�@Ϯ@�K�@�;d@�o@��@Ώ\@�V@�$�@��T@́@�G�@�?}@��@��`@̛�@ˮ@�@ʏ\@�E�@ɡ�@ȣ�@�bN@��m@�^5@ŉ7@�/@ēu@�9X@���@î@�C�@�o@�@�@�$�@�p�@��#@�@�-@��-@��^@�@���@�hs@���@�hs@��`@� �@�l�@��@��R@�^5@�$�@���@��T@��7@��u@�j@�I�@��F@�33@�~�@�O�@�Z@���@��y@�-@���@�%@���@��@�A�@�  @�S�@��!@�{@���@���@�hs@�/@�V@��@���@��
@�"�@���@�J@��^@��@�Z@�1'@��F@���@�v�@��\@�@���@�@��@��9@��
@��@�b@�1@���@�bN@�bN@�(�@�j@��u@�V@��`@�Ĝ@��m@��R@�-@��h@��@�@�$�@�E�@�M�@�-@���@���@�@�{@�J@�J@��#@���@�X@�&�@��@���@��u@� �@��m@���@�@��H@�ȴ@�M�@�=q@��@�@�O�@�?}@�&�@���@��@�ƨ@�
=@�~�@�{@���@���@�p�@�&�@��@�1'@�1@��m@���@�t�@�C�@���@��@��^@���@���@��h@��h@��h@�hs@��@���@�)�@���@y�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bNA�^5A�bNA�bNA�bNA�ZA�^5A�bNA�dZA�dZA�dZA�dZA�jA�l�A�n�A�n�A�p�A�p�A�p�A�n�A�l�A�l�A�n�A�n�A�p�A�p�A�p�A�n�A�l�A�n�A�t�A�t�A�r�A�bNA�C�A�bNA�A�ȴAʼjA��A��Aȉ7A��A�-A�r�A�bAŸRA�A�Q�A��mA���A��A�$�A��+A�bNA�K�A�;dA�(�A���A���A��DA�9XA�O�A��FA� �A�33A��A�1A��\A�|�A�JA�jA�(�A�A��9A��7A��A��A�ĜA��+A���A��FA�x�A�z�A�K�A�VA��A�r�A�A�5?A�Q�A��PA���A���A��
A�+A��PA�?}A�r�A��mA��A�A���A�S�A�9XA�^5A��A���A��A~�`Az��Au\)ArAl(�Ai�^Ag�TAfAd �A`ȴA\�RAZ��AW��AU�AUS�AS�TAQ/AO�AN�AM�#AL�/AL$�AJȴAF��AD��AB(�A?��A>��A;�wA9�TA8�yA7ƨA6��A5�hA4�A4=qA2�A/�A,��A,5?A+�^A*�A)VA(�A(ZA'�A'C�A&��A%&�A#�TA ��A jA�mA�/AoA�yA��AȴA��AS�A��A�A�RA�+A�A�A"�A��A�A�;A��A�uA33A��A1'A�\A�-A��A33A�A��AVA�A;dA
ZA	/A~�AȴA(�AA9XAJAA`BAoAĜA$�A  A�A�mA��AG�A%A �/A �DA {@�@��-@�Q�@�^5@��;@�/@�-@홚@�`B@�G�@�V@�`B@�+@�9X@�ƨ@���@�J@�+@�$�@���@�dZ@�\@�  @�l�@�u@�Q�@�F@���@�O�@�J@�!@旍@�\@旍@��@�7L@�&�@�Ĝ@��m@�v�@�Ĝ@� �@��;@ߕ�@���@��@�ȴ@���@݉7@�G�@�V@�j@���@�(�@ۮ@�\)@�C�@�
=@�E�@�G�@���@ؼj@�Z@�ƨ@׮@�dZ@��y@֗�@�5?@ա�@�/@��`@Դ9@�Q�@Ӿw@�t�@�S�@���@�@�%@�r�@Ϯ@�K�@�;d@�o@��@Ώ\@�V@�$�@��T@́@�G�@�?}@��@��`@̛�@ˮ@�@ʏ\@�E�@ɡ�@ȣ�@�bN@��m@�^5@ŉ7@�/@ēu@�9X@���@î@�C�@�o@�@�@�$�@�p�@��#@�@�-@��-@��^@�@���@�hs@���@�hs@��`@� �@�l�@��@��R@�^5@�$�@���@��T@��7@��u@�j@�I�@��F@�33@�~�@�O�@�Z@���@��y@�-@���@�%@���@��@�A�@�  @�S�@��!@�{@���@���@�hs@�/@�V@��@���@��
@�"�@���@�J@��^@��@�Z@�1'@��F@���@�v�@��\@�@���@�@��@��9@��
@��@�b@�1@���@�bN@�bN@�(�@�j@��u@�V@��`@�Ĝ@��m@��R@�-@��h@��@�@�$�@�E�@�M�@�-@���@���@�@�{@�J@�J@��#@���@�X@�&�@��@���@��u@� �@��m@���@�@��H@�ȴ@�M�@�=q@��@�@�O�@�?}@�&�@���@��@�ƨ@�
=@�~�@�{@���@���@�p�@�&�@��@�1'@�1@��m@���@�t�@�C�@���@��@��^@���@���@��h@��h@��h@�hs@��@���@�)�@���@y�D11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
%�B
'�B
'�B
,B
,B
-B
G�B
�BB+B;dB]/BiyBk�Bm�Bq�Bw�B}�B~�Bu�Bt�B~�B�1B�+B�B�hB��B�B�B�3B�qB��B��B��B�/B�)B�HB�yB�B�B�B�B��BB+B2-B+B,B(�B$�B%�B-B0!B+B!�B �B�BPB	7BB�B�HB��BɺB�}B�9B��B��B�Bw�BbNBM�B6FB�B	7B
��B
��B
�B
�B
aHB
?}B
{B	��B	��B	�XB	�hB	y�B	o�B	dZB	T�B	C�B	&�B	bB��B�B�B�TB�#B��B��B��BɺBŢB�qB�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�FB�LB�LB�XB�RB�RB�dB�XB�?B�qB�qB�dB�qBÖBŢBǮB��B��B��BǮBȴBǮBǮBǮBɺB��B��B��B��B��BɺB��B��B�sB	B		7B	
=B		7B		7B	1B	+B	%B	+B	B	B��B	
=B	\B	bB	oB	�B	�B	 �B	#�B	+B	-B	-B	-B	0!B	33B	7LB	:^B	=qB	?}B	B�B	C�B	E�B	?}B	49B	1'B	(�B	"�B	!�B	#�B	'�B	6FB	Q�B	^5B	aHB	cTB	dZB	l�B	q�B	r�B	t�B	u�B	�B	�B	t�B	v�B	w�B	x�B	|�B	�1B	�bB	�oB	�uB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�LB	�^B	�qB	��B	��B	B	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�/B	�5B	�5B	�NB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�`B	�ZB	�TB	�NB	�HB	�TB	�TB	�TB	�TB	�TB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�mB	�fB	�TB	�NB	�HB	�HB	�;B	�;B	�BB	�BB	�HB	�NB	�TB	�mB	�B	�B	�yB	�yB	�B	�B	�@5p�B	�B	��B	��B	��B	��B
B
B
%B
+B
B
  B
  B
  B
B
B
%B
%B
1B
1B
1B
	7B
	7B
	7B

=B

=B
DB
PB
VB
VB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
VB
PB
PB
PB
JB
JB
DB
DB
DB
DB
DB

=B

=B
DB
JB
PB
PB
PB
VB
VB
VB
VB
VB
oB
�B
�B
,�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
%�B
'�B
'�B
,B
,B
-B
G�B
�BB+B;dB]/BiyBk�Bm�Bq�Bw�B}�B~�Bu�Bt�B~�B�1B�+B�B�hB��B�B�B�3B�qB��B��B��B�/B�)B�HB�yB�B�B�B�B��BB+B2-B+B,B(�B$�B%�B-B0!B+B!�B �B�BPB	7BB�B�HB��BɺB�}B�9B��B��B�Bw�BbNBM�B6FB�B	7B
��B
��B
�B
�B
aHB
?}B
{B	��B	��B	�XB	�hB	y�B	o�B	dZB	T�B	C�B	&�B	bB��B�B�B�TB�#B��B��B��BɺBŢB�qB�9B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�FB�LB�LB�XB�RB�RB�dB�XB�?B�qB�qB�dB�qBÖBŢBǮB��B��B��BǮBȴBǮBǮBǮBɺB��B��B��B��B��BɺB��B��B�sB	B		7B	
=B		7B		7B	1B	+B	%B	+B	B	B��B	
=B	\B	bB	oB	�B	�B	 �B	#�B	+B	-B	-B	-B	0!B	33B	7LB	:^B	=qB	?}B	B�B	C�B	E�B	?}B	49B	1'B	(�B	"�B	!�B	#�B	'�B	6FB	Q�B	^5B	aHB	cTB	dZB	l�B	q�B	r�B	t�B	u�B	�B	�B	t�B	v�B	w�B	x�B	|�B	�1B	�bB	�oB	�uB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�LB	�^B	�qB	��B	��B	B	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�)B	�/B	�5B	�5B	�NB	�ZB	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�`B	�ZB	�TB	�NB	�HB	�TB	�TB	�TB	�TB	�TB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�mB	�fB	�TB	�NB	�HB	�HB	�;B	�;B	�BB	�BB	�HB	�NB	�TB	�mB	�B	�B	�yB	�yB	�B	�B	�@5p�B	�B	��B	��B	��B	��B
B
B
%B
+B
B
  B
  B
  B
B
B
%B
%B
1B
1B
1B
	7B
	7B
	7B

=B

=B
DB
PB
VB
VB
PB
PB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
bB
bB
bB
VB
PB
PB
PB
JB
JB
DB
DB
DB
DB
DB

=B

=B
DB
JB
PB
PB
PB
VB
VB
VB
VB
VB
oB
�B
�B
,�22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222242222222222222222222222222222222222222222222222222222222222222222222222222222G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190612                              AO  ARCAADJP                                                                    20181005190612    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190612  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190612  QCF$                G�O�G�O�G�O�8000            