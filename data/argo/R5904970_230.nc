CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:15:47Z creation      
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
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @L   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  B   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  J�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Qt   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  XP   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Z   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  `�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  b�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  r   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20181024141547  20181024141547  5904970 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6785                            2B  A   APEX                            7726                            111215                          846 @���}�1   @��UUh @6�A�7K��d1�����1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   B   @333@�  @�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DK��DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DVfDV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Dy�{D�H D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @:=p@��@ƸRAA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BHp�BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C)C)C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�GC�C��C�C�C�C�C�C�C�C�C�C�C��C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�GC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
D �
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D
D�
D	
D	�
D

D
�
D
D�
D
D�
D
D�
D
D�
D
D�
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
D�
D
D�
D
D�
D
D�
D
D�
D
D�pD
D�
D
D�
D
D�
D
D�
D 
D �
D!
D!�
D"
D"�
D#
D#�
D$
D$�
D%
D%�
D&
D&�
D'
D'�
D(
D(�
D)
D)�
D*
D*�
D+
D+�
D,
D,�
D-
D-�
D.
D.�
D/
D/�
D0
D0�
D1
D1�
D2
D2�
D3
D3�
D4
D4�
D5
D5�
D6
D6�
D7
D7�
D8
D8�
D9
D9�
D:
D:�
D;
D;�
D<
D<�
D=
D=�
D>
D>�
D?
D?�
D@
D@�
DApDA�
DB
DB�
DC
DC�
DD
DD�
DE
DE�
DF
DF�
DG
DG�
DH
DH�
DI
DI�
DJ
DJ�
DK
DK�
DL �DL�
DM
DM�
DN
DN�
DO
DO�
DP
DP�
DQ
DQ�
DR
DR�
DS
DS�
DT
DT�
DU
DU�
DVpDV�
DW
DW�
DX
DX�
DY
DY�
DZ
DZ�
D[
D[�
D\
D\�
D]
D]�
D^
D^�
D_
D_�
D`
D`�
Da
Da�
DbpDb�
Dc
Dc�
Dd
Dd�
De
De�
Dy��D�K�D��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��^A��RA��FA��!A���A���A��uA�z�A�r�A�S�A�E�A�E�A�C�A�?}A�=qA�;dA�=qA�=qA�;dA�;dA�?}A�C�A�E�A�C�A�?}A�A�A�E�A�G�A�E�A�G�A�G�A�K�A�O�A�Q�A�VA�S�A�S�A�XA�Q�A�G�A�E�A�A�A�;dA�9XA�7LA�33A�(�A�  A���A�C�A���A��A��#A��A��RA���A���A���A�M�A��\A��TA�x�A���A�ƨA�ZA�O�A�hsA���A���A�bA�Q�A�/A���A�5?A��A�&�A��A���A���A�9XA���A��FA��A�+A��`A�1A��A��7A���A���A��!A��yA���A�
=A��DA��A��RA��A���A���A��-A�1'A~�/A~A�A~JA}%Ax��AwhsAt��Ar�!Ao�Am\)Ak�^Aj��AiAh��Af��Ae�Ad��Adr�Ac�A`�uA_��A^5?A\��AZĜAY�mAX��AWG�AU�AS�^AQ�AO��AL��AKC�AJ5?AH�uAE+AD=qAC�TACO�AB^5AAp�A?�hA>5?A<�DA:�A9A8��A8�9A8(�A733A6-A41A1�;A0�A0z�A0 �A/��A/S�A/%A.��A.A�A+p�A)�PA(~�A'�7A&bA%&�A$=qA#x�A"E�A!x�A ��A��A��A�;At�A��AC�AbA�wA�A�9Az�A9XA��An�AJA�^AjA�A�A��A�wA33A��A�
A"�A�`A-A��A��An�A-A�A��A
�RA
VA
 �A
JA	�
A	�^A	dZA	Az�AA��Al�A��AM�A|�A|�A�+A�\A�uAx�@�^5@�Q�@��T@�z�@��+@�X@�|�@�v�@��T@�-@�P@�{@��#@�&�@�@�j@��@��@�@�@���@�S�@�dZ@�7L@�  @�dZ@�"�@���@��@ܴ9@�\)@�M�@ش9@��
@׾w@أ�@ؼj@�Z@֗�@���@�r�@��@�b@�v�@���@�V@� �@�33@���@�J@Ł@�z�@§�@�Ĝ@�Q�@�Q�@�9X@���@�S�@�+@�~�@�=q@��@��F@�t�@�o@���@�$�@�@�x�@���@�\)@���@���@�~�@�ff@�M�@���@���@�`B@�K�@�V@���@��F@�l�@���@���@�l�@��!@�J@�?}@��@�1'@�l�@�ȴ@�^5@��@���@�`B@���@��@�b@�o@��+@�v�@�$�@�x�@���@��m@��w@��F@�K�@���@���@���@�v�@��@��9@� �@��m@���@�l�@��@��@���@�~�@�V@���@�@��h@��7@�X@��@�%@���@��@�bN@�1'@��;@��F@��@���@��P@�dZ@�C�@�"�@�@�@��R@��!@���@��\@�~�@�ff@�$�@���@�`B@�O�@�?}@�/@���@��@�Z@�bN@� �@���@��;@���@�33@���@��R@�~�@�M�@�M�@�M�@�E�@�@�`B@��@���@��u@�j@�r�@�z�@��@�z�@�z�@��@��@�I�@��;@�\)@���@��@���@�7L@��@��/@�r�@�A�@�(�@� �@���@�l�@�33@�@��!@��+@���@�5?@y��@d�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��^A��RA��FA��!A���A���A��uA�z�A�r�A�S�A�E�A�E�A�C�A�?}A�=qA�;dA�=qA�=qA�;dA�;dA�?}A�C�A�E�A�C�A�?}A�A�A�E�A�G�A�E�A�G�A�G�A�K�A�O�A�Q�A�VA�S�A�S�A�XA�Q�A�G�A�E�A�A�A�;dA�9XA�7LA�33A�(�A�  A���A�C�A���A��A��#A��A��RA���A���A���A�M�A��\A��TA�x�A���A�ƨA�ZA�O�A�hsA���A���A�bA�Q�A�/A���A�5?A��A�&�A��A���A���A�9XA���A��FA��A�+A��`A�1A��A��7A���A���A��!A��yA���A�
=A��DA��A��RA��A���A���A��-A�1'A~�/A~A�A~JA}%Ax��AwhsAt��Ar�!Ao�Am\)Ak�^Aj��AiAh��Af��Ae�Ad��Adr�Ac�A`�uA_��A^5?A\��AZĜAY�mAX��AWG�AU�AS�^AQ�AO��AL��AKC�AJ5?AH�uAE+AD=qAC�TACO�AB^5AAp�A?�hA>5?A<�DA:�A9A8��A8�9A8(�A733A6-A41A1�;A0�A0z�A0 �A/��A/S�A/%A.��A.A�A+p�A)�PA(~�A'�7A&bA%&�A$=qA#x�A"E�A!x�A ��A��A��A�;At�A��AC�AbA�wA�A�9Az�A9XA��An�AJA�^AjA�A�A��A�wA33A��A�
A"�A�`A-A��A��An�A-A�A��A
�RA
VA
 �A
JA	�
A	�^A	dZA	Az�AA��Al�A��AM�A|�A|�A�+A�\A�uAx�@�^5@�Q�@��T@�z�@��+@�X@�|�@�v�@��T@�-@�P@�{@��#@�&�@�@�j@��@��@�@�@���@�S�@�dZ@�7L@�  @�dZ@�"�@���@��@ܴ9@�\)@�M�@ش9@��
@׾w@أ�@ؼj@�Z@֗�@���@�r�@��@�b@�v�@���@�V@� �@�33@���@�J@Ł@�z�@§�@�Ĝ@�Q�@�Q�@�9X@���@�S�@�+@�~�@�=q@��@��F@�t�@�o@���@�$�@�@�x�@���@�\)@���@���@�~�@�ff@�M�@���@���@�`B@�K�@�V@���@��F@�l�@���@���@�l�@��!@�J@�?}@��@�1'@�l�@�ȴ@�^5@��@���@�`B@���@��@�b@�o@��+@�v�@�$�@�x�@���@��m@��w@��F@�K�@���@���@���@�v�@��@��9@� �@��m@���@�l�@��@��@���@�~�@�V@���@�@��h@��7@�X@��@�%@���@��@�bN@�1'@��;@��F@��@���@��P@�dZ@�C�@�"�@�@�@��R@��!@���@��\@�~�@�ff@�$�@���@�`B@�O�@�?}@�/@���@��@�Z@�bN@� �@���@��;@���@�33@���@��R@�~�@�M�@�M�@�M�@�E�@�@�`B@��@���@��u@�j@�r�@�z�@��@�z�@�z�@��@��@�I�@��;@�\)@���@��@���@�7L@��@��/@�r�@�A�@�(�@� �@���@�l�@�33@�@��!@��+@���@�5?@y��@d�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BG�BG�BG�BF�BF�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BK�BL�BL�BL�BJ�BM�BM�BN�BN�BN�BO�BP�BQ�BR�BR�BR�BS�BVBXBYBYBZBZBZB[#B\)BiyB~�B�oB��B��B��B�FB�-B�FB��B��B�B��B��B��B�B�B�B�/B�jB��B��B�JBu�Bl�B`BBQ�BG�BE�BA�B?}B>wB;dB7LB5?B33B/B#�B{B��B��B�B�;B��BȴB�RB��B�uB�JB~�A�{B
��B
�wB
�!B
��B
��B
��B
�{B
�VB
r�B
e`B
Q�B
?}B
,B
�B

=B
B	��B	�B	�ZB	��B	��B	��B	ȴB	�!B	��B	��B	��B	�+B	}�B	t�B	k�B	dZB	bNB	VB	J�B	=qB	2-B	,B	(�B	�B	hB	\B	DB	%B	  B��B�B�fB�BB�5B�)B�#B�B��B��BǮB��B�dB�^B�RB�LB�FB�?B�3B�'B�B��B��B��B�uB�\B�PB�DB�7B�%B�B�B~�B|�Bz�Bw�Bv�Bp�Bp�Bn�Bm�Bl�Bk�Bk�BiyBhsBffBe`BbNB`BB_;B_;B]/B\)BZBYBW
BW
BS�BR�BR�BR�BQ�BQ�BP�BO�BN�BN�BM�BL�BL�BK�BK�BJ�BI�BI�BH�BE�BF�BC�BH�BL�BM�BM�BI�BC�BA�BB�B@�B=qB=qB>wB<jB;dB<jB9XB:^B>wBA�BC�BD�BE�BD�BD�B>wB9XB6FB0!B/B/B49B8RB@�BK�BT�B[#BffBgmBiyBs�Bv�Bw�B|�B{�Bz�B}�B{�Bz�Bx�Bx�By�Bz�B{�B|�B{�B|�B|�Bv�Bt�Bv�B�B�%B�+B�7B�DB�DB�VB�hB�uB��B��B��B��B��B��B��B��B�B�B�B�B�-B�9B�?BÖB�
B�BB�HB�NB�sB��B��B	B	+B	JB	bB	uB	�B	�B	 �B	#�B	%�B	'�B	)�B	-B	/B	6FB	8RB	8RB	:^B	?}B	D�B	H�B	I�B	I�B	K�B	P�B	P�B	P�B	R�B	\)B	aHB	e`B	ffB	gmB	iyB	k�B	n�B	r�B	t�B	u�B	w�B	y�B	z�B	z�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�+B	�1B	�1B	�7B	�7B	�=B	�=B	�=B	�=B	�=B	�=B	�DB	�PB	�PB	�PB	�VB	�VB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�?B	�XB	�qB	�}B	��B	��B	B	ŢB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	�B	�HB
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BG�BG�BG�BF�BF�BF�BF�BF�BF�BG�BG�BG�BG�BG�BG�BG�BH�BH�BH�BH�BI�BK�BL�BL�BL�BJ�BM�BM�BN�BN�BN�BO�BP�BQ�BR�BR�BR�BS�BVBXBYBYBZBZBZB[#B\)BiyB~�B�oB��B��B��B�FB�-B�FB��B��B�B��B��B��B�B�B�B�/B�jB��B��B�JBu�Bl�B`BBQ�BG�BE�BA�B?}B>wB;dB7LB5?B33B/B#�B{B��B��B�B�;B��BȴB�RB��B�uB�JB~�A�{B
��B
�wB
�!B
��B
��B
��B
�{B
�VB
r�B
e`B
Q�B
?}B
,B
�B

=B
B	��B	�B	�ZB	��B	��B	��B	ȴB	�!B	��B	��B	��B	�+B	}�B	t�B	k�B	dZB	bNB	VB	J�B	=qB	2-B	,B	(�B	�B	hB	\B	DB	%B	  B��B�B�fB�BB�5B�)B�#B�B��B��BǮB��B�dB�^B�RB�LB�FB�?B�3B�'B�B��B��B��B�uB�\B�PB�DB�7B�%B�B�B~�B|�Bz�Bw�Bv�Bp�Bp�Bn�Bm�Bl�Bk�Bk�BiyBhsBffBe`BbNB`BB_;B_;B]/B\)BZBYBW
BW
BS�BR�BR�BR�BQ�BQ�BP�BO�BN�BN�BM�BL�BL�BK�BK�BJ�BI�BI�BH�BE�BF�BC�BH�BL�BM�BM�BI�BC�BA�BB�B@�B=qB=qB>wB<jB;dB<jB9XB:^B>wBA�BC�BD�BE�BD�BD�B>wB9XB6FB0!B/B/B49B8RB@�BK�BT�B[#BffBgmBiyBs�Bv�Bw�B|�B{�Bz�B}�B{�Bz�Bx�Bx�By�Bz�B{�B|�B{�B|�B|�Bv�Bt�Bv�B�B�%B�+B�7B�DB�DB�VB�hB�uB��B��B��B��B��B��B��B��B�B�B�B�B�-B�9B�?BÖB�
B�BB�HB�NB�sB��B��B	B	+B	JB	bB	uB	�B	�B	 �B	#�B	%�B	'�B	)�B	-B	/B	6FB	8RB	8RB	:^B	?}B	D�B	H�B	I�B	I�B	K�B	P�B	P�B	P�B	R�B	\)B	aHB	e`B	ffB	gmB	iyB	k�B	n�B	r�B	t�B	u�B	w�B	y�B	z�B	z�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�%B	�+B	�1B	�1B	�7B	�7B	�=B	�=B	�=B	�=B	�=B	�=B	�DB	�PB	�PB	�PB	�VB	�VB	�VB	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�?B	�XB	�qB	�}B	��B	��B	B	ŢB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	�B	�HB
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141547                              AO  ARCAADJP                                                                    20181024141547    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141547  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141547  QCF$                G�O�G�O�G�O�0               