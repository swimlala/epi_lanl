CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:42Z creation      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20181024140842  20181024140842  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @�����1   @�ᥟJf@5;dZ��d	���l�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(ffB0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C)�fC,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN  CO�fCQ�fCS�fCV  CX  CZ  C\  C^  C_�fCb  Cd  Ce�fCh  Cj  Cl  Cm�fCo�fCq�fCt  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C�  C�  C�  C��3C��3C��3C�  C��3C��3C��3C�  C��C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD � D ��D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D�fD	  D	� D
  D
� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#�fD$  D$y�D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3y�D3��D4y�D5  D5� D6  D6� D6��D7� D8fD8� D9  D9� D:fD:� D;  D;� D<  D<� D=  D=� D=��D>y�D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DEfDE� DF  DF� DG  DG� DH  DH� DI  DIy�DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW�fDX  DX� DY  DY� DZ  DZ� D[  D[� D\fD\�fD]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dt  Dt� DufDu� Dv  Dv� DwfDw� Dw��Dy�fD�0�D�F�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @+�@�@�A�HA"�HAB�HAb�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B �RB�RB�RB�RB!�B)�B0�RB8�RB@�RBH�RBP�RBX�RB`�RBh�RBp�RBx�RB�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)Bď\B�B�(�B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)B�\)C .CzC.CG�C.C
.C.C.C.C.C.C.C.C.C.C.C .C".C$.C&.C(.C*zC,.C..C0.C2.C4.C6.C8.C:.C<.C>.C@.CB.CD.CF.CH.CJ.CLG�CN.CPzCRzCTzCV.CX.CZ.C\.C^.C`zCb.Cd.CfzCh.Cj.Cl.CnzCpzCrzCt.Cv.Cx.Cz.C|.C~.C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�
=C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
=C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�
C�
C�
C�
=C�
C�
C�
=C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�
C�
C�
C�#�C�
C�
C�
C�
C�#�C�
C�
C�
C�
=C�
=C�
=C�
C�
=C�
=C�
=C�
C�#�C�
C�
=C�
C�
C�#�C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
C�
D �D ��DD��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��DD��D�D��D�D��D�D��D�D�D�D��D�D��D�D��D�D��D�D��D D ��D!�D!��D"�D"��D#�D#��D$�D$�D%D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3D3�D4D4�D5�D5��D6�D6��D7D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>D>�D?�D?��D@�D@��DA�DA��DB�DB��DC�DC�DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI�DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��DeDe��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds�Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dw�RDy��D�6gD�L{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AͰ!AͰ!Aͥ�A͝�Aͣ�AͲ-AͰ!AͶFAͺ^Aͺ^AͼjA;wAͼjAͺ^AͼjAͥ�A̓A�M�A���A̩�A̍PÁA�hsA�K�A�C�A�C�A�I�A�v�A̲-A̙�A�9XA��A���A˝�AˁA�p�A�hsA�C�A���A�O�A�+AǛ�AƶFAś�A���A�p�A�bA�VA�(�A��A�dZA���A��yA�r�A�ȴA��TA���A�&�A�ȴA��#A�t�A�bA�7LA�I�A��A��uA�n�A��A���A��FA�ffA��9A�=qA�%A��A��jA���A��!A���A���A��jA��A�K�A���A��#A�&�A��A�v�A�+A��A��A�-A�dZA��A��PA�O�A�5?A�I�A��A�ƨA�ĜA�x�A��uA~�A{�
AzjAxZAv��Ar(�Aq?}An�`Al�Ak��Aj�HAjr�Ah�Ag|�Af��Ae��Aa�A]l�AZ��AYVAUK�AShsASG�AS�AR�jAR(�AQ�^AQ;dAPM�AO?}AL�yAL5?AK�mAKS�AJ^5AI�7AH�`AG��AF��ADA�AB�+AA�-AAA@�RA?��A?�hA?�7A?A>(�A=��A;�;A;%A:(�A8�A7hsA6~�A4�`A4bNA3�wA2�!A2bA1p�A01A/S�A.bA+��A*�A*$�A(��A&��A&��A&~�A&^5A&=qA%;dA$�A#?}A!ƨA �A�AK�A��A1'AO�A7LA��A$�A�A��A�A�FA��A��A��A`BA�\A`BAr�A��Ap�A�AȴA
��A��A�`A�AĜA�!A��A�\A�TA ^5@��@�1'@�t�@��@�%@�9X@��@��F@�+@���@�&�@�bN@��@�w@�dZ@�@���@�+@�^@�@�;d@���@��`@�Z@��
@�S�@��@��#@�%@�F@�{@ߥ�@���@��/@�b@܋D@�Z@�Q�@��@�V@�V@ְ!@�ȴ@�J@�x�@���@Դ9@Ԭ@�  @���@ӥ�@ѩ�@Л�@ЋD@Ͼw@�v�@͙�@���@�Z@�1@�S�@ɲ-@�  @��/@�n�@��@�$�@�G�@��@�dZ@�`B@�v�@��j@�J@���@�|�@���@��^@��D@�(�@��@�ƨ@��w@��w@��@�@�$�@��-@�%@��D@���@�o@���@�{@���@���@���@�dZ@�K�@�33@��@���@��;@��@��/@���@��@� �@���@���@��!@�+@�\)@��
@�A�@�Z@� �@���@�z�@��@�v�@���@���@�v�@��@�@�O�@��/@���@� �@�|�@�\)@�C�@�ȴ@��#@�hs@�?}@�&�@�&�@���@�Z@�  @�1@��@��;@���@��@�S�@�
=@��\@���@��^@�hs@��@��/@���@�r�@�Z@�b@�  @��m@���@��P@�t�@�;d@�o@��y@�{@�$�@��-@�G�@���@��@�bN@�A�@� �@�1@�  @��;@���@���@�  @���@�l�@�@��H@�n�@�5?@�V@�-@��@���@��h@�p�@�`B@��@���@��@��j@�Z@�A�@�(�@��m@��@�t�@�t�@�S�@�;d@�
=@��H@���@���@���@�^5@���@��@���@��j@�A�@��;@��w@�l�@�@��!@�v�@�$�@���@���@�?}@�7L@�/@�&�@���@��j@��@�A�@� �@��@�1@��m@��w@�K�@�@��@���@��+@�v�@�n�@�n�@�V@�E�@�-@�@�@��^@���@�`B@�&�@���@��`@��/@���@�r�@�  @��
@���@���@�ƨ@��@��@�S�@�o@���@�ȴ@��!@�ff@��@���@�@��h@�O�@�w�@{6z@ot�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AͰ!AͰ!Aͥ�A͝�Aͣ�AͲ-AͰ!AͶFAͺ^Aͺ^AͼjA;wAͼjAͺ^AͼjAͥ�A̓A�M�A���A̩�A̍PÁA�hsA�K�A�C�A�C�A�I�A�v�A̲-A̙�A�9XA��A���A˝�AˁA�p�A�hsA�C�A���A�O�A�+AǛ�AƶFAś�A���A�p�A�bA�VA�(�A��A�dZA���A��yA�r�A�ȴA��TA���A�&�A�ȴA��#A�t�A�bA�7LA�I�A��A��uA�n�A��A���A��FA�ffA��9A�=qA�%A��A��jA���A��!A���A���A��jA��A�K�A���A��#A�&�A��A�v�A�+A��A��A�-A�dZA��A��PA�O�A�5?A�I�A��A�ƨA�ĜA�x�A��uA~�A{�
AzjAxZAv��Ar(�Aq?}An�`Al�Ak��Aj�HAjr�Ah�Ag|�Af��Ae��Aa�A]l�AZ��AYVAUK�AShsASG�AS�AR�jAR(�AQ�^AQ;dAPM�AO?}AL�yAL5?AK�mAKS�AJ^5AI�7AH�`AG��AF��ADA�AB�+AA�-AAA@�RA?��A?�hA?�7A?A>(�A=��A;�;A;%A:(�A8�A7hsA6~�A4�`A4bNA3�wA2�!A2bA1p�A01A/S�A.bA+��A*�A*$�A(��A&��A&��A&~�A&^5A&=qA%;dA$�A#?}A!ƨA �A�AK�A��A1'AO�A7LA��A$�A�A��A�A�FA��A��A��A`BA�\A`BAr�A��Ap�A�AȴA
��A��A�`A�AĜA�!A��A�\A�TA ^5@��@�1'@�t�@��@�%@�9X@��@��F@�+@���@�&�@�bN@��@�w@�dZ@�@���@�+@�^@�@�;d@���@��`@�Z@��
@�S�@��@��#@�%@�F@�{@ߥ�@���@��/@�b@܋D@�Z@�Q�@��@�V@�V@ְ!@�ȴ@�J@�x�@���@Դ9@Ԭ@�  @���@ӥ�@ѩ�@Л�@ЋD@Ͼw@�v�@͙�@���@�Z@�1@�S�@ɲ-@�  @��/@�n�@��@�$�@�G�@��@�dZ@�`B@�v�@��j@�J@���@�|�@���@��^@��D@�(�@��@�ƨ@��w@��w@��@�@�$�@��-@�%@��D@���@�o@���@�{@���@���@���@�dZ@�K�@�33@��@���@��;@��@��/@���@��@� �@���@���@��!@�+@�\)@��
@�A�@�Z@� �@���@�z�@��@�v�@���@���@�v�@��@�@�O�@��/@���@� �@�|�@�\)@�C�@�ȴ@��#@�hs@�?}@�&�@�&�@���@�Z@�  @�1@��@��;@���@��@�S�@�
=@��\@���@��^@�hs@��@��/@���@�r�@�Z@�b@�  @��m@���@��P@�t�@�;d@�o@��y@�{@�$�@��-@�G�@���@��@�bN@�A�@� �@�1@�  @��;@���@���@�  @���@�l�@�@��H@�n�@�5?@�V@�-@��@���@��h@�p�@�`B@��@���@��@��j@�Z@�A�@�(�@��m@��@�t�@�t�@�S�@�;d@�
=@��H@���@���@���@�^5@���@��@���@��j@�A�@��;@��w@�l�@�@��!@�v�@�$�@���@���@�?}@�7L@�/@�&�@���@��j@��@�A�@� �@��@�1@��m@��w@�K�@�@��@���@��+@�v�@�n�@�n�@�V@�E�@�-@�@�@��^@���@�`B@�&�@���@��`@��/@���@�r�@�  @��
@���@���@�ƨ@��@��@�S�@�o@���@�ȴ@��!@�ff@��@���@�@��h@�O�@�w�@{6z@ot�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B �B �B �B �B#�B'�B7LBA�BO�BaHBp�Bu�Bu�Bz�B~�B�B�1B�hB��B��B�B�B  BB+BDBPBVBoB�B!�B/B<jBH�B\)BffBgmBdZBgmBjBk�Bo�B{�B}�B�B�%B�7B�hB�{B�uB�uB�uB�oB�PB�+B�B}�Bs�Bp�Bk�B`BBP�B?}B2-B%�B�B�BbB��B��B�fBɺB�wB�3B��B�{Br�BcTBK�BF�B?}B49B,B!�BPB
�ZB
��B
�}B
�3B
�B
��B
��B
� B
\)B
>wB
0!B
$�B
hB
B	�fB	�)B	��B	�jB	�9B	�B	��B	��B	��B	�uB	�7B	jB	C�B	,B	�B	B��B��B�B�B�B�B�sB�`B�HB�B�B��B��B��B��B��B��BɺBɺB��B�
B�;B�NB�`B�fB�`B�`B�TB�HB�HB�NB�HB�5B�B�B��B��B��BȴBĜB��B�RB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�PB�DB�+B�B�B�B�B�B� B|�B{�Bz�Bx�Bx�B{�B�B�B�B~�B~�B}�Bw�Br�BiyBXBP�BP�BP�BP�BP�BN�BO�BP�BO�BN�BP�BYBhsBq�Bx�B�B�B�VB�bB�hB�hB�oB�oB�oB�hB�hB��B��B��B��B��B�{B�{B�uB�oB�\B�DB�%B�B�B�B�B�hB��B��B��B��B��B��B��B��B��B��B��B�B�LB�LB�LB�LB�LB�LB�FB�LB�RB�RB�RB�^B�wBBĜB�jB�-B�B�?B�FB��B��B��B�PB�JB�bB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�qBĜBƨB��B��B�
B�TB�yB�B�B�B��B	1B	VB	hB	uB	�B	-B	33B	9XB	B�B	F�B	G�B	J�B	M�B	O�B	ZB	_;B	cTB	dZB	hsB	l�B	jB	iyB	jB	k�B	m�B	o�B	o�B	r�B	u�B	v�B	v�B	y�B	~�B	�B	�B	�B	�B	�B	�=B	�PB	�PB	�PB	�PB	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�3B	�?B	�LB	�RB	�RB	�XB	�^B	�^B	�dB	�jB	�qB	��B	ÖB	ÖB	ÖB	B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�5B	�;B	�;B	�BB	�NB	�TB	�TB	�TB	�TB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
	7B

=B

=B

=B

=B

=B

=B
DB
JB
JB
PB
PB
VB
bB
bB
bB
bB
{B
&B
�B
$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B �B �B �B �B#�B'�B7LBA�BO�BaHBp�Bu�Bu�Bz�B~�B�B�1B�hB��B��B�B�B  BB+BDBPBVBoB�B!�B/B<jBH�B\)BffBgmBdZBgmBjBk�Bo�B{�B}�B�B�%B�7B�hB�{B�uB�uB�uB�oB�PB�+B�B}�Bs�Bp�Bk�B`BBP�B?}B2-B%�B�B�BbB��B��B�fBɺB�wB�3B��B�{Br�BcTBK�BF�B?}B49B,B!�BPB
�ZB
��B
�}B
�3B
�B
��B
��B
� B
\)B
>wB
0!B
$�B
hB
B	�fB	�)B	��B	�jB	�9B	�B	��B	��B	��B	�uB	�7B	jB	C�B	,B	�B	B��B��B�B�B�B�B�sB�`B�HB�B�B��B��B��B��B��B��BɺBɺB��B�
B�;B�NB�`B�fB�`B�`B�TB�HB�HB�NB�HB�5B�B�B��B��B��BȴBĜB��B�RB�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�PB�DB�+B�B�B�B�B�B� B|�B{�Bz�Bx�Bx�B{�B�B�B�B~�B~�B}�Bw�Br�BiyBXBP�BP�BP�BP�BP�BN�BO�BP�BO�BN�BP�BYBhsBq�Bx�B�B�B�VB�bB�hB�hB�oB�oB�oB�hB�hB��B��B��B��B��B�{B�{B�uB�oB�\B�DB�%B�B�B�B�B�hB��B��B��B��B��B��B��B��B��B��B��B�B�LB�LB�LB�LB�LB�LB�FB�LB�RB�RB�RB�^B�wBBĜB�jB�-B�B�?B�FB��B��B��B�PB�JB�bB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�'B�?B�qBĜBƨB��B��B�
B�TB�yB�B�B�B��B	1B	VB	hB	uB	�B	-B	33B	9XB	B�B	F�B	G�B	J�B	M�B	O�B	ZB	_;B	cTB	dZB	hsB	l�B	jB	iyB	jB	k�B	m�B	o�B	o�B	r�B	u�B	v�B	v�B	y�B	~�B	�B	�B	�B	�B	�B	�=B	�PB	�PB	�PB	�PB	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�-B	�-B	�3B	�?B	�LB	�RB	�RB	�XB	�^B	�^B	�dB	�jB	�qB	��B	ÖB	ÖB	ÖB	B	ŢB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�5B	�;B	�;B	�BB	�NB	�TB	�TB	�TB	�TB	�`B	�fB	�fB	�fB	�fB	�mB	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
+B
+B
+B
	7B

=B

=B

=B

=B

=B

=B
DB
JB
JB
PB
PB
VB
bB
bB
bB
bB
{B
&B
�B
$�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.18 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140842                              AO  ARCAADJP                                                                    20181024140842    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140842  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140842  QCF$                G�O�G�O�G�O�0               