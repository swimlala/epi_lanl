CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:08:13Z creation      
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
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20181024140813  20181024140813  5904957 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               /A   AO  6560                            2B  A   APEX                            7471                            062512                          846 @׻dZ�1   @׻d��c�@3U?|�h�c��;dZ1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      /A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�33B�  B�  B�  C   C  C�C�C  C
  C  C  C  C  C�fC�fC�fC  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C��D   D y�D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D�fD	fD	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D   D � D ��D!� D"  D"� D#  D#y�D$  D$� D%  D%� D&  D&y�D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D+��D,� D-  D-� D.  D.� D/  D/� D0  D0�fD0��D1� D2  D2� D3  D3� D4  D4� D5  D5� D6fD6� D7  D7� D8  D8�fD9fD9� D:  D:�fD;fD;� D<  D<� D=  D=�fD>fD>� D?  D?� D?��D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DL��DM� DN  DN� DN��DO� DP  DP� DQ  DQ� DRfDR�fDSfDS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX�fDY  DY� DZ  DZy�DZ��D[� D\  D\� D]  D]�fD^  D^y�D^��D_� D`  D`�fDafDay�Da��Db� Dc  Dc�fDd  Dd� De  De� Df  Df� DgfDg�fDh  Dh� Dh��Diy�Dj  Dj� Dk  Dk� Dl  Dl�fDm  Dm� Dn  Dn� Do  Doy�Do��Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� Dw��Dy�
D�A�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�G�@��HAp�A!p�AAp�Aap�A��RA��RA��RA��RA��RAиRA�RA�RB \)B\)B\)B\)B B(\)B0\)B8\)B@\)BH\)BP\)BX\)B`\)Bh\)Bp\)Bx\)B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�.B�aGB�aGB�aGB�.B�.B�.B�aGB�.B�.B�.C 
C
C0�C0�C
C

C
C
C
C
C�pC�pC�pC
C
C
C 
C"
C$
C&
C(
C*0�C,
C.
C0
C2
C4
C6
C8
C:
C<
C>
C@
CB
CD
CF
CH
CJ
CL
CM�pCP
CR
CT
CV
CX
CZ
C\
C^
C`
Cb
Cd
Cf
Ch
Cj
Cl
Cn
Cp
Cr
Ct
Cv
Cx
Cz
C|
C~
C��C��C��C��C�RC�RC��C��C�RC��C��C��C��C��C��C��C��C��C��C��C��C���C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C���C��C�RC��C��C�RC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC�RC��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�RC��C��C��C��C��C��C��C��C�RC�RD �D ]D�D��D�D��D�D��D�D��D�D��D�D]D�D��D�D�)D	)D	�)D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�]D��D�D��D�D��D�D��D�D�)D�D��D�D��D�D��D�D��D�D��D)D��D�D��D�D��D�D��D �D ��D �]D!��D"�D"��D#�D#]D$�D$��D%�D%��D&�D&]D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D+�]D,��D-�D-��D.�D.��D/�D/��D0�D0�)D0�]D1��D2�D2��D3�D3��D4�D4��D5�D5��D6)D6��D7�D7��D8�D8�)D9)D9��D:�D:�)D;)D;��D<�D<��D=�D=�)D>)D>��D?�D?��D?�]D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DL�]DM��DN�DN��DN�]DO��DP�DP��DQ�DQ��DR)DR�)DS)DS�)DT�DT��DU�DU��DV�DV��DW�DW��DX�DX�)DY�DY��DZ�DZ]DZ�]D[��D\�D\��D]�D]�)D^�D^]D^�]D_��D`�D`�)Da)Da]Da�]Db��Dc�Dc�)Dd�Dd��De�De��Df�Df��Dg)Dg�)Dh�Dh��Dh�]Di]Dj�Dj��Dk�Dk��Dl�Dl�)Dm�Dm��Dn�Dn��Do�Do]Do�]Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�)Du�Du��Dv�Dv��Dw�Dw��Dw�]Dy��D�D�D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A׺^A�XA���A�  A�t�A�bA�|�A�VAӸRAӧ�A�l�A�-A�bNA��A��TA�jA̙�A�VA�7LA�O�A��`A�r�A���A�z�Aƺ^Aź^A��`AþwA�`BA��A�oA�JA��A�|�A��A�A�A��^A�{A�XA���A�XA�ĜA�M�A��#A�C�A���A�ȴA�n�A�t�A���A��A��!A�1'A��DA�A��A�v�A�n�A���A�oA���A��hA��/A�;dA��
A���A��RA��A�9XA��A�9XA�/A�x�A�?}A�%A��;A��RA�r�A�x�A��A���A�oA��;A��A�K�A�ffA�K�A�S�A��A���A���A���A�7LA��yA�jAz�+As��Ao&�Ak�Af��Aa��A_�-A^�A]VA[/AYx�AWhsAU?}AT �AS�
ASƨAR�AP��AOK�AM\)AJ  AHQ�AGXAF{AD�AA��A?t�A=�FA:�uA9�A8�\A7��A7A6��A6�+A5�A4��A4=qA37LA1��A0{A.��A-��A,�yA,�!A,A*��A&�A$  A#"�A!��A �`A ��A   A�/A(�Av�AĜA��A/A$�A�DAA�PA-A�\A��A~�A�;Ap�A~�Az�AdZA��A�`A��AbA\)A"�A
=@��^@���@���@�r�@�P@�@�x�@�%@�@��@���@�j@�!@�V@�j@��H@�O�@��;@�V@�-@�7L@�j@ާ�@���@�1@�ƨ@�V@Չ7@�bN@���@ҏ\@�^5@��@�/@��@υ@�K�@�ȴ@�5?@��@͙�@��`@�b@˥�@�|�@�33@��H@ʧ�@�=q@���@�V@�j@�|�@�K�@�K�@�33@�
=@��y@�V@��@���@ź^@ŉ7@ċD@���@�;d@�@�J@��7@�&�@��`@��D@� �@�ƨ@�\)@�"�@��@�@��!@��\@��@�`B@�/@���@�j@�|�@�n�@�V@�=q@��@��T@���@�p�@�1'@���@�V@�{@���@�X@�?}@���@��/@���@�bN@�  @���@���@�33@��!@��\@�v�@�ff@�=q@�p�@���@��D@�1'@���@���@�o@���@���@�v�@��@��h@���@��w@��H@�$�@��^@�x�@�G�@�hs@�G�@�O�@�7L@��@���@�|�@�S�@�o@���@���@���@��+@�{@�hs@�?}@�7L@���@��u@�Q�@��@��m@��F@��@�\)@��@��R@���@�-@��#@���@��7@�/@���@��j@��D@�r�@�r�@�z�@�z�@��@�z�@�r�@�bN@�9X@�9X@��@���@�|�@�t�@�t�@�;d@��!@�^5@�$�@���@�%@���@�I�@�b@��
@�"�@���@�M�@��^@���@��h@��h@�`B@��@�Z@��@�1@��w@���@��P@��@�;d@��y@��R@��!@��\@�M�@���@�?}@��@��u@�1'@��
@�+@�E�@��T@��^@�x�@�7L@�%@���@���@��@���@���@���@���@���@��+@�5?@�hs@��/@�r�@��@��@��;@��
@���@�|�@�l�@�33@�ȴ@�~�@�^5@�E�@�=q@�=q@�{@���@��^@���@�bN@�1'@�b@���@���@��P@�|�@��@���@�~�@�^5@�M�@�5?@���@�X@�7L@��j@��@�z�@�j@�A�@�b@�dZ@�;d@���@�E�@�@��@��T@��#@��-@��7@�O�@���@��@�Z@��@r)�@e�d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A׺^A�XA���A�  A�t�A�bA�|�A�VAӸRAӧ�A�l�A�-A�bNA��A��TA�jA̙�A�VA�7LA�O�A��`A�r�A���A�z�Aƺ^Aź^A��`AþwA�`BA��A�oA�JA��A�|�A��A�A�A��^A�{A�XA���A�XA�ĜA�M�A��#A�C�A���A�ȴA�n�A�t�A���A��A��!A�1'A��DA�A��A�v�A�n�A���A�oA���A��hA��/A�;dA��
A���A��RA��A�9XA��A�9XA�/A�x�A�?}A�%A��;A��RA�r�A�x�A��A���A�oA��;A��A�K�A�ffA�K�A�S�A��A���A���A���A�7LA��yA�jAz�+As��Ao&�Ak�Af��Aa��A_�-A^�A]VA[/AYx�AWhsAU?}AT �AS�
ASƨAR�AP��AOK�AM\)AJ  AHQ�AGXAF{AD�AA��A?t�A=�FA:�uA9�A8�\A7��A7A6��A6�+A5�A4��A4=qA37LA1��A0{A.��A-��A,�yA,�!A,A*��A&�A$  A#"�A!��A �`A ��A   A�/A(�Av�AĜA��A/A$�A�DAA�PA-A�\A��A~�A�;Ap�A~�Az�AdZA��A�`A��AbA\)A"�A
=@��^@���@���@�r�@�P@�@�x�@�%@�@��@���@�j@�!@�V@�j@��H@�O�@��;@�V@�-@�7L@�j@ާ�@���@�1@�ƨ@�V@Չ7@�bN@���@ҏ\@�^5@��@�/@��@υ@�K�@�ȴ@�5?@��@͙�@��`@�b@˥�@�|�@�33@��H@ʧ�@�=q@���@�V@�j@�|�@�K�@�K�@�33@�
=@��y@�V@��@���@ź^@ŉ7@ċD@���@�;d@�@�J@��7@�&�@��`@��D@� �@�ƨ@�\)@�"�@��@�@��!@��\@��@�`B@�/@���@�j@�|�@�n�@�V@�=q@��@��T@���@�p�@�1'@���@�V@�{@���@�X@�?}@���@��/@���@�bN@�  @���@���@�33@��!@��\@�v�@�ff@�=q@�p�@���@��D@�1'@���@���@�o@���@���@�v�@��@��h@���@��w@��H@�$�@��^@�x�@�G�@�hs@�G�@�O�@�7L@��@���@�|�@�S�@�o@���@���@���@��+@�{@�hs@�?}@�7L@���@��u@�Q�@��@��m@��F@��@�\)@��@��R@���@�-@��#@���@��7@�/@���@��j@��D@�r�@�r�@�z�@�z�@��@�z�@�r�@�bN@�9X@�9X@��@���@�|�@�t�@�t�@�;d@��!@�^5@�$�@���@�%@���@�I�@�b@��
@�"�@���@�M�@��^@���@��h@��h@�`B@��@�Z@��@�1@��w@���@��P@��@�;d@��y@��R@��!@��\@�M�@���@�?}@��@��u@�1'@��
@�+@�E�@��T@��^@�x�@�7L@�%@���@���@��@���@���@���@���@���@��+@�5?@�hs@��/@�r�@��@��@��;@��
@���@�|�@�l�@�33@�ȴ@�~�@�^5@�E�@�=q@�=q@�{@���@��^@���@�bN@�1'@�b@���@���@��P@�|�@��@���@�~�@�^5@�M�@�5?@���@�X@�7L@��j@��@�z�@�j@�A�@�b@�dZ@�;d@���@�E�@�@��@��T@��#@��-@��7@�O�@���@��@�Z@��@r)�@e�d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B�B�LBɺB�B�HB�fB�B�B��BBDB{B �B"�B"�B"�B$�B)�B2-BI�BR�B[#BcTBe`Bo�B~�B�!B�}BĜBȴB��B��BŢB�qB�-B��B��B��B��B��B��B�{B�\B�%B�B�B}�Bs�BW
B�BB��B��B�B�HB�B��B��BȴBƨBĜB�}B�9B��B�{B�+Be`BA�B:^B,B�BB
��B
�sB
�B
ǮB
��B
dZB
>wB
B	�B	�qB	��B	�JB	iyB	_;B	YB	L�B	B�B	<jB	2-B	&�B	 �B	 �B	 �B	�B	�B	bB	B�B�ZB�5B�B�
B��B��BǮBB�}B�}BBĜBƨBǮBǮBĜBB��B��B��B�}B�wB�qB�jB�^B�3B��B�VB�%B�B~�B}�B{�Bx�Bt�Bn�BffBcTBk�B�B�VB��B�'B�dBƨBɺB��B^VB�-BƨB��B��B��B��B��BɺBƨBB�LB�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�3B�9B�3B�3B�-B�B��B�B�3B�9B�LB�XB�XB�XB�^B�jBBĜBĜBƨBȴBɺB��B��B��B��B��B��B�B�
B�B�B�#B�)B�;B�BB�HB�NB�TB�ZB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	  B	B	B	B	B	B	B	1B	VB	\B	bB	bB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	.B	0!B	1'B	33B	49B	49B	6FB	6FB	7LB	8RB	;dB	;dB	=qB	?}B	B�B	C�B	D�B	D�B	E�B	L�B	O�B	Q�B	S�B	T�B	XB	\)B	^5B	^5B	`BB	bNB	ffB	k�B	t�B	z�B	�B	�%B	�1B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�^B	�dB	�jB	�qB	�}B	�}B	��B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�#B	�)B	�5B	�5B	�5B	�5B	�5B	�BB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
	7B
	7B
JB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
\B
bB
hB
hB
oB
oB
oB
oB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#B
)�B
331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B�B�B��B��B��B��B�B�LBɺB�B�HB�fB�B�B��BBDB{B �B"�B"�B"�B$�B)�B2-BI�BR�B[#BcTBe`Bo�B~�B�!B�}BĜBȴB��B��BŢB�qB�-B��B��B��B��B��B��B�{B�\B�%B�B�B}�Bs�BW
B�BB��B��B�B�HB�B��B��BȴBƨBĜB�}B�9B��B�{B�+Be`BA�B:^B,B�BB
��B
�sB
�B
ǮB
��B
dZB
>wB
B	�B	�qB	��B	�JB	iyB	_;B	YB	L�B	B�B	<jB	2-B	&�B	 �B	 �B	 �B	�B	�B	bB	B�B�ZB�5B�B�
B��B��BǮBB�}B�}BBĜBƨBǮBǮBĜBB��B��B��B�}B�wB�qB�jB�^B�3B��B�VB�%B�B~�B}�B{�Bx�Bt�Bn�BffBcTBk�B�B�VB��B�'B�dBƨBɺB��B^VB�-BƨB��B��B��B��B��BɺBƨBB�LB�B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�-B�3B�9B�3B�3B�-B�B��B�B�3B�9B�LB�XB�XB�XB�^B�jBBĜBĜBƨBȴBɺB��B��B��B��B��B��B�B�
B�B�B�#B�)B�;B�BB�HB�NB�TB�ZB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B	  B	  B	B	B	B	B	B	B	1B	VB	\B	bB	bB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	.B	0!B	1'B	33B	49B	49B	6FB	6FB	7LB	8RB	;dB	;dB	=qB	?}B	B�B	C�B	D�B	D�B	E�B	L�B	O�B	Q�B	S�B	T�B	XB	\)B	^5B	^5B	`BB	bNB	ffB	k�B	t�B	z�B	�B	�%B	�1B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�3B	�9B	�9B	�?B	�FB	�LB	�RB	�^B	�dB	�jB	�qB	�}B	�}B	��B	ÖB	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�#B	�)B	�5B	�5B	�5B	�5B	�5B	�BB	�NB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
1B
	7B
	7B
JB
PB
PB
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
\B
bB
hB
hB
oB
oB
oB
oB
oB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#B
)�B
331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.09 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024140813                              AO  ARCAADJP                                                                    20181024140813    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024140813  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024140813  QCF$                G�O�G�O�G�O�0               