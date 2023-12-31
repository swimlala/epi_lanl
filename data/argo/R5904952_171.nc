CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:05:44Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20181005190544  20181005190544  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)�q�1   @��*[,@12-V�c�
=p��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @@  @�  @�  A   A!��A@  A`  A�  A���A�  A�  A���A�  A�  A�33A�33B  B  B  B ffB(ffB0  B8  B@  BH  BP  BX  B`  BhffBp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  C   C�C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Ck�fCm�fCp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C��3C�  C��3C�  C�  C�  C�  C��3C�  C�  C��3C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C��C�  C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��C��C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C��C��C��C�  D   D � D  Dy�D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D	fD	� D	��D
� DfD� D��D� DfD� D  D� D  D� D  D� D  Dy�D  D� D  D�fDfD� D  Dy�D  D� D  D�fD  D� D  D�fD  D� D��D� D  D� D  D� D  D� D  D� D fD �fD!  D!� D"fD"� D#  D#� D$  D$y�D$��D%� D&  D&� D'  D'� D(  D(y�D(��D)y�D*  D*� D+fD+� D+��D,y�D-  D-� D.  D.� D/  D/y�D0  D0�fD1fD1� D1��D2� D3  D3�fD4  D4� D5fD5� D5��D6� D7  D7� D8  D8y�D9  D9�fD:  D:� D;  D;y�D;��D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC�fDD  DD� DEfDE� DF  DFy�DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DK��DL� DMfDM� DN  DN� DOfDO� DP  DP� DQ  DQ�fDRfDR�fDS  DS� DS��DTy�DT��DU� DVfDV�fDWfDW�fDXfDX� DX��DY� DZfDZ�fD[  D[� D\  D\y�D]  D]� D^  D^� D_  D_� D_��D`y�D`��Day�Da��Db� Dc  Dc� Dc��Ddy�Dd��De� DffDf�fDg  Dgy�Dh  Dh�fDi  Di� Dj  Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Do��Dpy�Dp��Dqy�Dr  Dry�Dr��Dsy�Ds��Dty�Du  Du�fDv  Dv� Dv��Dw� Dw��Dy�HD�J�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @Q�@���@���Az�A&{ADz�Adz�A�=qA�
>A�=qA�=qA�
>A�=qA�=qA�p�B �RB	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��\B��\B��\B��\B��\B�B��\B��\B��\B�B��\B��\B��\B�\)B��\B��\B��\Bď\Bȏ\B̏\B�\)Bԏ\B؏\B܏\B��\B�B�\B�\B�\)B�\B��\B��\C G�CaHC.CG�CG�C
G�CG�CG�CG�CG�CG�CG�CG�CG�CG�CG�C G�C"G�C$G�C&G�C(G�C*G�C,G�C.G�C0G�C2G�C4aHC6G�C8G�C:G�C<G�C>G�C@G�CBG�CDG�CFG�CHG�CJG�CLG�CNG�CPG�CRG�CTG�CVG�CX.CZG�C\G�C^G�C`G�CbG�CdG�CfG�ChG�CjG�Cl.Cn.CpG�CrG�CtG�CvG�CxG�CzG�C|G�C~G�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�
C�
C�#�C�
C�#�C�#�C�#�C�#�C�
C�#�C�#�C�
C�#�C�0�C�0�C�0�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�#�C�
C�#�C�#�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�
C�#�C�0�C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�#�C�0�C�0�C�#�C�#�C�0�C�0�C�#�C�#�C�#�C�#�C�0�C�#�C�#�C�#�C�#�C�#�C�
C�
C�#�C�#�C�#�C�0�C�0�C�0�C�0�C�#�D �D ��D�D��D�D��D�D��D�D��D�D��D�D�RDRD��D�D��D	RD	��D
�D
��DRD��D�D��DRD��D�D��D�D��D�D��D�D��D�D��D�D�RDRD��D�D��D�D��D�D�RD�D��D�D�RD�D��D�D��D�D��D�D��D�D��D�D��D RD �RD!�D!��D"RD"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+RD+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0�RD1RD1��D2�D2��D3�D3�RD4�D4��D5RD5��D6�D6��D7�D7��D8�D8��D9�D9�RD:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DARDA��DB�DB��DC�DC�RDD�DD��DERDE��DF�DF��DG�DG��DH�DH�RDI�DI��DJ�DJ��DK�DK��DL�DL��DMRDM��DN�DN��DORDO��DP�DP��DQ�DQ�RDRRDR�RDS�DS��DT�DT��DU�DU��DVRDV�RDWRDW�RDXRDX��DY�DY��DZRDZ�RD[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��DfRDf�RDg�Dg��Dh�Dh�RDi�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du�RDv�Dv��Dw�Dw��Dw޹Dy�4D�S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��yA��A��A��A��yA���A���A���A���A���A���A���A��A���A���A�  A���A���A���A���A���A�
=A�JA�JAχ+A�p�A��/A��;AоwA�|�A�O�A�A�A��A���AϸRA�r�A�"�AζFA�VA�1'A�  A�x�A�=qA˛�A�~�A�;dA�&�A�AʾwAʬAʩ�AʁA�`BA�ZA�I�A�/A�oA��mA� �A�M�Aě�A�r�A��RA���A��A�jA��A��!A��A���A�9XA�VA��A�9XA�5?A�C�A�&�A�ȴA�VA�x�A��A�=qA��A�l�A�{A�r�A��A�/A��DA�1A�$�A���A��A��jA�A��HA��
A���A��A�33A�hsA��A�A�ƨA�x�Az-Av�9As�Ap �Ao/AmAl��AljAj �Ah�jAfI�Ac|�Aat�A`��A`(�A^��A[AYp�AV1'AR�yAQAO�AL�AKl�AI+AE��AA�7A?"�A=�A;�A9�PA8�HA6�HA4bNA2��A0bNA/K�A.~�A-��A,��A+��A*��A)��A)�A'��A%&�A#�A#t�A#"�A"��A"E�A!ƨA!?}A M�A&�AE�A`BAK�AVA�\AbA�TAA\)A�HA�#A�jA5?A�
A"�AI�A�HA�-A��A�#A=qA�Az�A�A	�
AffA33AVA|�A33AAr�AAO�AZA��AdZA+A �yA @�ff@�&�@���@���@��@�  @�K�@�~�@�=q@�%@�S�@�@��@��@�&�@�@@��^@�S�@���@�5?@�/@���@�K�@�X@�b@�"�@�=q@��@ݩ�@�hs@�j@�C�@��@��`@ׅ@֗�@Ցh@�(�@���@���@�/@�z�@�bN@��@�ƨ@�|�@�S�@�33@��H@�~�@�{@��@�1'@ˮ@�o@�G�@��@�Ĝ@ȃ@��@ǝ�@Ǯ@ǍP@�
=@�~�@�@���@�j@�  @ÍP@�o@¸R@+@�n�@�ff@�^5@��@�G�@��@�I�@�r�@��@�\)@�S�@��H@�{@���@�M�@�$�@�@�x�@��7@��^@�x�@�/@���@� �@��P@�@��!@�^5@�{@��@�V@���@��j@��@��P@�l�@��@��@�o@��+@��-@���@��@�?}@��@�9X@��@�t�@�+@�@��H@��T@���@��w@��\@�V@��^@���@�Q�@��@��;@��w@���@��@�S�@�33@�o@�
=@���@��R@�ȴ@��R@���@���@�n�@�@�9X@�9X@��@�1@�  @���@��F@�C�@��@�^5@�5?@�{@���@���@�p�@��`@�z�@�9X@�\)@�n�@�v�@��+@�M�@��@���@�p�@���@��@�z�@� �@�  @�  @��;@�t�@��@���@�v�@�E�@���@�hs@�X@�/@���@��9@�r�@���@��
@��
@�ƨ@�|�@��@��H@��H@��@��H@�~�@�n�@�n�@�ff@�^5@�M�@�M�@�5?@�$�@�J@��@�@��@�O�@�&�@���@�1'@��
@���@�;d@���@�V@�{@��@���@�G�@��`@�z�@�r�@�j@���@�|�@�S�@�"�@��@�~�@�^5@�=q@���@��#@��#@�@��7@�`B@�V@��/@�z�@��@���@��w@�S�@���@�n�@�5?@���@���@�p�@�&�@�Ĝ@��D@�Z@�Q�@�(�@��@�ƨ@��@�C�@�"�@��R@�n�@�M�@�$�@��@��-@���@��h@�G�@��@���@��j@��@�r�@� �@�ƨ@��P@�\)@�+@��@���@��H@��!@�,=@}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��yA��A��A��A��yA���A���A���A���A���A���A���A��A���A���A�  A���A���A���A���A���A�
=A�JA�JAχ+A�p�A��/A��;AоwA�|�A�O�A�A�A��A���AϸRA�r�A�"�AζFA�VA�1'A�  A�x�A�=qA˛�A�~�A�;dA�&�A�AʾwAʬAʩ�AʁA�`BA�ZA�I�A�/A�oA��mA� �A�M�Aě�A�r�A��RA���A��A�jA��A��!A��A���A�9XA�VA��A�9XA�5?A�C�A�&�A�ȴA�VA�x�A��A�=qA��A�l�A�{A�r�A��A�/A��DA�1A�$�A���A��A��jA�A��HA��
A���A��A�33A�hsA��A�A�ƨA�x�Az-Av�9As�Ap �Ao/AmAl��AljAj �Ah�jAfI�Ac|�Aat�A`��A`(�A^��A[AYp�AV1'AR�yAQAO�AL�AKl�AI+AE��AA�7A?"�A=�A;�A9�PA8�HA6�HA4bNA2��A0bNA/K�A.~�A-��A,��A+��A*��A)��A)�A'��A%&�A#�A#t�A#"�A"��A"E�A!ƨA!?}A M�A&�AE�A`BAK�AVA�\AbA�TAA\)A�HA�#A�jA5?A�
A"�AI�A�HA�-A��A�#A=qA�Az�A�A	�
AffA33AVA|�A33AAr�AAO�AZA��AdZA+A �yA @�ff@�&�@���@���@��@�  @�K�@�~�@�=q@�%@�S�@�@��@��@�&�@�@@��^@�S�@���@�5?@�/@���@�K�@�X@�b@�"�@�=q@��@ݩ�@�hs@�j@�C�@��@��`@ׅ@֗�@Ցh@�(�@���@���@�/@�z�@�bN@��@�ƨ@�|�@�S�@�33@��H@�~�@�{@��@�1'@ˮ@�o@�G�@��@�Ĝ@ȃ@��@ǝ�@Ǯ@ǍP@�
=@�~�@�@���@�j@�  @ÍP@�o@¸R@+@�n�@�ff@�^5@��@�G�@��@�I�@�r�@��@�\)@�S�@��H@�{@���@�M�@�$�@�@�x�@��7@��^@�x�@�/@���@� �@��P@�@��!@�^5@�{@��@�V@���@��j@��@��P@�l�@��@��@�o@��+@��-@���@��@�?}@��@�9X@��@�t�@�+@�@��H@��T@���@��w@��\@�V@��^@���@�Q�@��@��;@��w@���@��@�S�@�33@�o@�
=@���@��R@�ȴ@��R@���@���@�n�@�@�9X@�9X@��@�1@�  @���@��F@�C�@��@�^5@�5?@�{@���@���@�p�@��`@�z�@�9X@�\)@�n�@�v�@��+@�M�@��@���@�p�@���@��@�z�@� �@�  @�  @��;@�t�@��@���@�v�@�E�@���@�hs@�X@�/@���@��9@�r�@���@��
@��
@�ƨ@�|�@��@��H@��H@��@��H@�~�@�n�@�n�@�ff@�^5@�M�@�M�@�5?@�$�@�J@��@�@��@�O�@�&�@���@�1'@��
@���@�;d@���@�V@�{@��@���@�G�@��`@�z�@�r�@�j@���@�|�@�S�@�"�@��@�~�@�^5@�=q@���@��#@��#@�@��7@�`B@�V@��/@�z�@��@���@��w@�S�@���@�n�@�5?@���@���@�p�@�&�@�Ĝ@��D@�Z@�Q�@�(�@��@�ƨ@��@�C�@�"�@��R@�n�@�M�@�$�@��@��-@���@��h@�G�@��@���@��j@��@�r�@� �@�ƨ@��P@�\)@�+@��@���@��H@��!@�,=@}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bl�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bo�Bp�Bp�Bn�Bo�Bp�Bq�Bo�Bo�Bn�Bl�Bl�Bx�By�Bw�B�NB
n�BhB+BhB(�B.B33B;dBG�BQ�BVB[#BhsBm�Bp�Bq�BbNBG�B8RB9XBF�BJ�BN�BS�BT�BT�BYB_;B`BBaHBe`BhsB�%B��B�BÖB�BJB�B,B49BE�BQ�BR�BS�BQ�BbNBgmBdZBcTB_;BXBS�BP�BK�B>wB49B"�BB�B��B�dB��B�oB�=Bk�B6FB�B
��B
�#B
�9B
��B
�DB
XB
B�B
7LB
/B
)�B
�B	�B	��B	��B	�DB	x�B	t�B	q�B	k�B	dZB	R�B	H�B	K�B	L�B	B�B	>wB	:^B	2-B	!�B	hB��B�B�5B��BĜB�dB�B��B�B�FB�LB�^B�XB�FB�'B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�?B�^B�dB�dB�dB�}BǮBȴB��B��B��B��B��B��B��B��B��B�B�B�NB�NB�HB�HB�BB�)B�#B�/B�/B�)B�#B�/B�`B�`B�ZB�ZB�TB�TB�`B�NB�/B�;B�TB�mB�yB�yB�yB�B�B�B�B�B�B��B	B	DB	\B	\B	\B	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	)�B	-B	.B	1'B	33B	33B	49B	6FB	7LB	9XB	<jB	B�B	F�B	H�B	I�B	I�B	J�B	L�B	P�B	P�B	VB	\)B	_;B	`BB	aHB	aHB	bNB	dZB	ffB	gmB	l�B	l�B	m�B	q�B	t�B	s�B	t�B	y�B	z�B	z�B	{�B	{�B	}�B	|�B	|�B	|�B	� B	�B	�B	�B	�B	�B	�+B	�+B	�=B	�PB	�PB	�PB	�VB	�\B	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�-B	�3B	�3B	�9B	�9B	�9B	�?B	�?B	�9B	�9B	�9B	�XB	�^B	�^B	�^B	�dB	�dB	�qB	�}B	�}B	��B	��B	��B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�HB	�NB	�ZB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B
DB
JB
JB
JB
PB
PB
VB
VB
VB
\B
\B
uB
B
%�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  Bl�Bm�Bm�Bm�Bm�Bn�Bn�Bn�Bn�Bo�Bp�Bp�Bn�Bo�Bp�Bq�Bo�Bo�Bn�Bl�Bl�Bx�By�Bw�B�NB
n�BhB+BhB(�B.B33B;dBG�BQ�BVB[#BhsBm�Bp�Bq�BbNBG�B8RB9XBF�BJ�BN�BS�BT�BT�BYB_;B`BBaHBe`BhsB�%B��B�BÖB�BJB�B,B49BE�BQ�BR�BS�BQ�BbNBgmBdZBcTB_;BXBS�BP�BK�B>wB49B"�BB�B��B�dB��B�oB�=Bk�B6FB�B
��B
�#B
�9B
��B
�DB
XB
B�B
7LB
/B
)�B
�B	�B	��B	��B	�DB	x�B	t�B	q�B	k�B	dZB	R�B	H�B	K�B	L�B	B�B	>wB	:^B	2-B	!�B	hB��B�B�5B��BĜB�dB�B��B�B�FB�LB�^B�XB�FB�'B��B��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�?B�^B�dB�dB�dB�}BǮBȴB��B��B��B��B��B��B��B��B��B�B�B�NB�NB�HB�HB�BB�)B�#B�/B�/B�)B�#B�/B�`B�`B�ZB�ZB�TB�TB�`B�NB�/B�;B�TB�mB�yB�yB�yB�B�B�B�B�B�B��B	B	DB	\B	\B	\B	oB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	)�B	-B	.B	1'B	33B	33B	49B	6FB	7LB	9XB	<jB	B�B	F�B	H�B	I�B	I�B	J�B	L�B	P�B	P�B	VB	\)B	_;B	`BB	aHB	aHB	bNB	dZB	ffB	gmB	l�B	l�B	m�B	q�B	t�B	s�B	t�B	y�B	z�B	z�B	{�B	{�B	}�B	|�B	|�B	|�B	� B	�B	�B	�B	�B	�B	�+B	�+B	�=B	�PB	�PB	�PB	�VB	�\B	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�-B	�3B	�3B	�9B	�9B	�9B	�?B	�?B	�9B	�9B	�9B	�XB	�^B	�^B	�^B	�dB	�dB	�qB	�}B	�}B	��B	��B	��B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�;B	�HB	�NB	�ZB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B

=B

=B

=B

=B
DB
JB
JB
JB
PB
PB
VB
VB
VB
\B
\B
uB
B
%�222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.28 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190544                              AO  ARCAADJP                                                                    20181005190544    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190544  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190544  QCF$                G�O�G�O�G�O�8000            