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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^|   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fH   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �T   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181005190547  20181005190547  5904952 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6431                            2B  A   APEX                            7466                            062512                          846 @��)��Lk1   @��*>��&@1�������c�bM��1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @@  @�  @�  A��A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�ffB���B���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C	�fC  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB�CD  CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C��3C�  C��C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��Dy�D��D	y�D
  D
� D  D� D  D� D��D� D  Dy�D  D�fD  Dy�D��Dy�D  D�fDfD�fDfD� D  D� D  Dy�D��Dy�D��Dy�D  Dy�D��Dy�D��Dy�D  D� D  D� D��Dy�D��D� D   D � D!  D!� D!��D"� D#  D#� D$fD$�fD%fD%� D&  D&�fD'  D'� D(  D(� D)  D)� D)��D*� D+fD+�fD,  D,y�D-  D-� D.  D.� D/  D/� D0  D0y�D1  D1� D1��D2y�D2��D3� D4fD4�fD5fD5�fD6  D6�fD7  D7y�D7��D8� D9  D9� D:  D:y�D:��D;� D<fD<�fD=  D=� D=��D>y�D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DFy�DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DLy�DM  DM� DM��DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DWfDW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]fD]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc�fDd  Dd� De  De� De��Df� Dg  Dgy�Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� DlfDl� Dm  Dm�fDn  Dn� Do  Do� Dp  Dp� Dq  Dq� DrfDr�fDs  Dsy�Dt  Dt� DufDu� DvfDv�fDw  Dwy�Dw��Dy��D�H D��\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @S�
@��@��A�]A$��AD��Ad��A�z�A�z�A�G�A�z�A�z�A�z�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB1=qB9=qBA=qBI=qBQ=qBY=qBa=qBi=qBq=qBy=qB���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���BĞ�B���B̞�BО�B�B�k�B�k�B���B䞸B螸B잸B�B���B���B���C O\CO\CO\CO\CO\C
5�CO\CO\CO\C5�CO\CO\CO\CO\CO\CO\C O\C"O\C$O\C&O\C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8h�C:O\C<O\C>O\C@O\CBh�CDO\CF5�CHO\CJO\CLO\CNO\CPO\CRO\CTO\CVO\CXO\CZO\C\O\C^O\C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\CxO\CzO\C|O\C~O\C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C��C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C��C��C�'�C�'�C��C�'�C�'�C�'�C�'�C�4{C�4{C�'�C�'�C�'�C��C�'�C�4{C�'�C�4{C�4{C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�D �D ��DqD��D�D��D�D��D�D��D�D��D�D��D�D�qDqD�qD	qD	�qD
�D
��D�D��D�D��DqD��D�D�qD�D�=D�D�qDqD�qD�D�=D=D�=D=D��D�D��D�D�qDqD�qDqD�qD�D�qDqD�qDqD�qD�D��D�D��DqD�qDqD��D �D ��D!�D!��D"qD"��D#�D#��D$=D$�=D%=D%��D&�D&�=D'�D'��D(�D(��D)�D)��D*qD*��D+=D+�=D,�D,�qD-�D-��D.�D.��D/�D/��D0�D0�qD1�D1��D2qD2�qD3qD3��D4=D4�=D5=D5�=D6�D6�=D7�D7�qD8qD8��D9�D9��D:�D:�qD;qD;��D<=D<�=D=�D=��D>qD>�qD?�D?��D@�D@�qDA�DA��DB�DB��DC�DC��DD=DD��DE�DE��DF�DF�qDG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL�qDM�DM��DNqDN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS�=DT�DT��DU�DU��DV�DV��DW=DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]=D]��D^�D^��D_�D_��D`�D`��Da�Da��Db=Db��Dc�Dc�=Dd�Dd��De�De��DfqDf��Dg�Dg�qDhqDh��Di�Di��Dj�Dj��Dk�Dk��Dl=Dl��Dm�Dm�=Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr=Dr�=Ds�Ds�qDt�Dt��Du=Du��Dv=Dv�=Dw�Dw�qDw�Dy��D�Q�D��H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�=qA�K�A�I�A�K�A�K�A�S�A�Q�A�S�A�S�A�VA�S�A�S�A�S�A�Q�A�S�A�VA�VA�XAʰ!A�A��A�5?A˥�A��
A��
Aʏ\A�hsA�t�AʸRA��TA̩�A��A���A���A���A�
=A�%A�%A���A��`A���A̺^A̸RA̺^A̸RA̗�A��Aˉ7Aʛ�A���A��A�p�A���A��A��HA���A��FA�\)A��A��A��9A��A���A�-A��A���A��A���A�x�A��yA�G�A�1'A��HA���A��A�Q�A���A�x�A��A��mA��FA�^5A�$�A���A���A�G�A�l�A��`A�|�A���A�v�A���A��uA�l�A���A�A�ȴA�A�S�A�dZA��PA�`BA�(�A���A��hA�M�A�;dA��;A�v�A���A�XA~~�Az9XAuAr��Ak�hAeXA_l�A^�jA\jAY�FAV�`AU/AQƨAOO�AK�#AGƨAFffADbNA@9XA<^5A;A9�A8VA7��A7\)A6�!A5?}A3l�A1�A0jA.�9A,��A+`BA*��A)��A'�hA'
=A&��A%�A$1A#?}A"��A!�A!XA�wA��A5?A�wAS�A�!AG�A��AA�!A-A��AA�A$�A��A��A�hA�AS�A(�A�A�FA
��A
Q�A	\)A$�AXA�jA5?AA�FA�AA��AA�HA�A�^AC�A�!A ��@�C�@�=q@�G�@�;d@�hs@���@�%@���@��P@��@��#@���@�t�@���@�V@�7@�A�@�dZ@��y@�R@�n�@�h@�I�@�$�@�-@���@�@���@�K�@��@�?}@�bN@�n�@�p�@���@ܼj@� �@�l�@��y@�5?@ّh@�%@��@���@��@�(�@�o@�v�@�E�@��@�`B@Л�@�(�@��@�^5@��@�&�@�(�@ˍP@�S�@��@���@��H@�v�@�$�@ɑh@�V@ȃ@�bN@�b@��m@Ǖ�@�S�@Ƈ+@��@�?}@�Q�@��@��;@�dZ@°!@���@��@�Ĝ@�r�@�9X@���@���@�;d@�33@���@�$�@���@�/@���@�r�@���@��
@���@��w@��m@���@�@�ȴ@���@�~�@�v�@�n�@�^5@�{@��-@��h@��7@�X@�j@���@��@�\)@�+@���@��7@�/@�Ĝ@�l�@���@�5?@�{@��@��-@��7@�hs@�?}@�hs@���@�O�@��`@���@�9X@��
@���@���@�o@�$�@�@��T@�%@��j@���@���@��D@�z�@�1'@�;d@��R@�^5@�J@���@���@���@��h@��7@��@��@��@�`B@�&�@��@���@�r�@�Z@�Q�@�A�@���@�ƨ@��@���@��P@�t�@�K�@�
=@���@�E�@�$�@��@���@��T@�@��h@�x�@�V@���@���@��u@��@��@�r�@�Q�@�9X@� �@� �@� �@�(�@�9X@�Q�@�b@�|�@�+@��y@�ff@��@���@��#@��^@��h@�hs@�X@�&�@�%@��`@�Ĝ@���@�r�@�Z@�I�@� �@�ƨ@�33@�ȴ@�ff@�$�@��@���@��@�bN@�Q�@��;@�"�@��@��\@�5?@���@�O�@�V@��@�r�@��@�b@�1@���@���@��F@���@�C�@��R@��R@��!@�~�@�J@��@�hs@�&�@���@���@��9@��D@��D@�A�@�(�@��@�1@���@��m@�C�@���@���@���@�ȴ@�J@�`B@�V@��9@�z�@�9X@��;@��@�33@���@��!@��+@�^5@�$�@���@��^@�`B@�%@��u@��@�ƨ@��w@���@��v@�P�@gt�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�=qA�K�A�I�A�K�A�K�A�S�A�Q�A�S�A�S�A�VA�S�A�S�A�S�A�Q�A�S�A�VA�VA�XAʰ!A�A��A�5?A˥�A��
A��
Aʏ\A�hsA�t�AʸRA��TA̩�A��A���A���A���A�
=A�%A�%A���A��`A���A̺^A̸RA̺^A̸RA̗�A��Aˉ7Aʛ�A���A��A�p�A���A��A��HA���A��FA�\)A��A��A��9A��A���A�-A��A���A��A���A�x�A��yA�G�A�1'A��HA���A��A�Q�A���A�x�A��A��mA��FA�^5A�$�A���A���A�G�A�l�A��`A�|�A���A�v�A���A��uA�l�A���A�A�ȴA�A�S�A�dZA��PA�`BA�(�A���A��hA�M�A�;dA��;A�v�A���A�XA~~�Az9XAuAr��Ak�hAeXA_l�A^�jA\jAY�FAV�`AU/AQƨAOO�AK�#AGƨAFffADbNA@9XA<^5A;A9�A8VA7��A7\)A6�!A5?}A3l�A1�A0jA.�9A,��A+`BA*��A)��A'�hA'
=A&��A%�A$1A#?}A"��A!�A!XA�wA��A5?A�wAS�A�!AG�A��AA�!A-A��AA�A$�A��A��A�hA�AS�A(�A�A�FA
��A
Q�A	\)A$�AXA�jA5?AA�FA�AA��AA�HA�A�^AC�A�!A ��@�C�@�=q@�G�@�;d@�hs@���@�%@���@��P@��@��#@���@�t�@���@�V@�7@�A�@�dZ@��y@�R@�n�@�h@�I�@�$�@�-@���@�@���@�K�@��@�?}@�bN@�n�@�p�@���@ܼj@� �@�l�@��y@�5?@ّh@�%@��@���@��@�(�@�o@�v�@�E�@��@�`B@Л�@�(�@��@�^5@��@�&�@�(�@ˍP@�S�@��@���@��H@�v�@�$�@ɑh@�V@ȃ@�bN@�b@��m@Ǖ�@�S�@Ƈ+@��@�?}@�Q�@��@��;@�dZ@°!@���@��@�Ĝ@�r�@�9X@���@���@�;d@�33@���@�$�@���@�/@���@�r�@���@��
@���@��w@��m@���@�@�ȴ@���@�~�@�v�@�n�@�^5@�{@��-@��h@��7@�X@�j@���@��@�\)@�+@���@��7@�/@�Ĝ@�l�@���@�5?@�{@��@��-@��7@�hs@�?}@�hs@���@�O�@��`@���@�9X@��
@���@���@�o@�$�@�@��T@�%@��j@���@���@��D@�z�@�1'@�;d@��R@�^5@�J@���@���@���@��h@��7@��@��@��@�`B@�&�@��@���@�r�@�Z@�Q�@�A�@���@�ƨ@��@���@��P@�t�@�K�@�
=@���@�E�@�$�@��@���@��T@�@��h@�x�@�V@���@���@��u@��@��@�r�@�Q�@�9X@� �@� �@� �@�(�@�9X@�Q�@�b@�|�@�+@��y@�ff@��@���@��#@��^@��h@�hs@�X@�&�@�%@��`@�Ĝ@���@�r�@�Z@�I�@� �@�ƨ@�33@�ȴ@�ff@�$�@��@���@��@�bN@�Q�@��;@�"�@��@��\@�5?@���@�O�@�V@��@�r�@��@�b@�1@���@���@��F@���@�C�@��R@��R@��!@�~�@�J@��@�hs@�&�@���@���@��9@��D@��D@�A�@�(�@��@�1@���@��m@�C�@���@���@���@�ȴ@�J@�`B@�V@��9@�z�@�9X@��;@��@�33@���@��!@��+@�^5@�$�@���@��^@�`B@�%@��u@��@�ƨ@��w@���@��v@�P�@gt�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�fB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�B	'�B	e`B	n�B	u�B	��B	�B	�3B	�B	��B	�B	�}B
%B
A�B
^5B
hsB
u�B
��B
�FB
ĜB
��B
�B
�#B
�B
��B�B�B#�B9XBI�B[#Bl�B�B�JB�uB��B��B�XB�yBB+B+BoB�B0!B33B1'B5?B9XB<jBA�BB�BC�BD�BD�BD�BE�BM�BP�BR�BXBXB[#B[#BZBS�BA�B<jB"�BJB��B�sB�qB�\BhsBN�B8RB"�B�BB
�ZB
��B
�9B
��B
��B
��B
��B
~�B
[#B
=qB
%�B
PB	�B	��B	�dB	��B	�\B	}�B	YB	7LB	�B	oB	B��B�B�NB�#B��BĜB�LB�?B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�-B�9B�!B�B�B��B�B�'B�3B�XB�}B��B��B�XB�FB�LB��B��B��B��B�B��BŢBŢBŢBŢBȴB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�)B�;B�BB�TB�HB�TB�ZB�`B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	
=B	hB	{B	�B	�B	�B	�B	�B	"�B	#�B	'�B	)�B	)�B	)�B	,B	-B	,B	/B	2-B	33B	5?B	49B	49B	6FB	9XB	;dB	>wB	?}B	A�B	D�B	K�B	N�B	S�B	S�B	T�B	VB	ZB	`BB	bNB	cTB	ffB	hsB	jB	m�B	u�B	w�B	y�B	{�B	|�B	{�B	y�B	z�B	x�B	x�B	y�B	{�B	|�B	~�B	~�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�7B	�\B	�hB	�uB	�{B	�{B	�{B	�uB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�FB	�LB	�LB	�RB	�RB	�RB	�RB	�RB	�XB	�^B	�jB	�qB	�wB	�}B	�}B	�}B	��B	B	B	B	B	B	B	ÖB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�fB	�sB	�sB	�mB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
	7B
�B
"�B
1�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�fB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�sB�B	'�B	e`B	n�B	u�B	��B	�B	�3B	�B	��B	�B	�}B
%B
A�B
^5B
hsB
u�B
��B
�FB
ĜB
��B
�B
�#B
�B
��B�B�B#�B9XBI�B[#Bl�B�B�JB�uB��B��B�XB�yBB+B+BoB�B0!B33B1'B5?B9XB<jBA�BB�BC�BD�BD�BD�BE�BM�BP�BR�BXBXB[#B[#BZBS�BA�B<jB"�BJB��B�sB�qB�\BhsBN�B8RB"�B�BB
�ZB
��B
�9B
��B
��B
��B
��B
~�B
[#B
=qB
%�B
PB	�B	��B	�dB	��B	�\B	}�B	YB	7LB	�B	oB	B��B�B�NB�#B��BĜB�LB�?B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�-B�-B�9B�!B�B�B��B�B�'B�3B�XB�}B��B��B�XB�FB�LB��B��B��B��B�B��BŢBŢBŢBŢBȴB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�)B�;B�BB�TB�HB�TB�ZB�`B�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	
=B	hB	{B	�B	�B	�B	�B	�B	"�B	#�B	'�B	)�B	)�B	)�B	,B	-B	,B	/B	2-B	33B	5?B	49B	49B	6FB	9XB	;dB	>wB	?}B	A�B	D�B	K�B	N�B	S�B	S�B	T�B	VB	ZB	`BB	bNB	cTB	ffB	hsB	jB	m�B	u�B	w�B	y�B	{�B	|�B	{�B	y�B	z�B	x�B	x�B	y�B	{�B	|�B	~�B	~�B	}�B	� B	�B	�B	�B	�B	�B	�B	�B	�7B	�\B	�hB	�uB	�{B	�{B	�{B	�uB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�'B	�3B	�9B	�FB	�LB	�LB	�RB	�RB	�RB	�RB	�RB	�XB	�^B	�jB	�qB	�wB	�}B	�}B	�}B	��B	B	B	B	B	B	B	ÖB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�BB	�BB	�HB	�NB	�NB	�TB	�TB	�TB	�TB	�ZB	�fB	�sB	�sB	�mB	�mB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
	7B
�B
"�B
1�2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.31 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005190547                              AO  ARCAADJP                                                                    20181005190547    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005190547  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005190547  QCF$                G�O�G�O�G�O�8000            