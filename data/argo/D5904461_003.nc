CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:20Z AOML 3.0 creation; 2016-08-07T21:36:27Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150226221320  20160807143627  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_003                   2C  D   APEX                            6531                            072314                          846 @�U"��1   @�U�I��@1!���o�c��z�H1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �CffC��C  C  C	�fC�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�Dy�fD�3D�P D�y�D���D� D�P D�i�D��3D���D�L�D�y�DǙ�D�	�D�6fDډ�D��3D��D�0 D�|�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�Q�@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�G�A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��Bx��B�Q�B�Q�B�Q�B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��RBÅB��B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�C B�C�\C��C(�C(�C
\C\C(�C(�C(�C(�C(�C(�C(�C(�C(�C (�C"(�C$(�C&(�C((�C*B�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:(�C<(�C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CT(�CV(�CX(�CZ(�C\(�C^(�C`(�Cb(�Cd(�Cf(�Ch(�Cj(�Cl(�Cn(�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~(�C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�{D 
=D �=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D	
=D	�=D

=D
�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D
=D�=D 
=D �=D!
=D!�=D"
=D"�=D#
=D#�=D$
=D$�=D%
=D%�=D&
=D&�=D'
=D'�=D(
=D(�=D)
=D)�=D*
=D*�=D+
=D+�=D,
=D,�=D-
=D-�=D.
=D.�=D/
=D/�=D0
=D0�=D1
=D1�=D2
=D2�=D3
=D3�=D4
=D4�=D5
=D5�=D6
=D6�=D7
=D7�=D8
=D8�=D9
=D9�=D:
=D:�=D;
=D;�=D<
=D<�=D=
=D=�=D>
=D>�=D?
=D?�=D@
=D@�=DA
=DA�=DB
=DB�=DC�DC�=DD
=DD�=DE
=DE�=DF
=DF�=DG
=DG�=DH
=DH�=DI
=DI�=DJ
=DJ�=DK
=DK�=DL
=DL�=DM
=DM�=DN
=DN�=DO
=DO�=DP
=DP�=DQ
=DQ�=DR
=DR�=DS
=DS�=DT
=DT�=DU
=DU�=DV
=DV�=DW
=DW�=DX
=DX�=DY
=DY�=DZ
=DZ�=D[
=D[�=D\
=D\�=D]
=D]�=D^
=D^�=D_
=D_�=D`
=D`�=Da
=Da�=Db
=Db�=Dc
=Dc�=Dd
=Dd�=De
=De�=Df
=Df�=Dg
=Dg�=Dh
=Dh�=Di
=Di�=Dj
=Dj�=Dk
=Dk�=Dl
=Dl�=Dm
=Dm�=Dn
=Dn�=Do
=Do�=Dp
=Dp�=Dq
=Dq�=Dr
=Dr�=Ds
=Ds�=Dt
=Dtw
Dy��D�RD�UD�~�D���D�D�UD�n�D��RD��D�Q�D�~�DǞ�D��D�;�Dڎ�D��RD��D�5D��D�˅11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�K�A�G�A�G�A�G�A�?}A�;dA�;dA�-A�1'A� �A��A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ĜA�ƨA�ĜA�AܾwAܺ^Aܴ9Aܡ�A�~�A�^5A�A�A�VA��#AۍPA�AجA�p�A�9XA�~�AҋDAϰ!Aˣ�A�VAǡ�A��A�oA�jA��mAć+A�VA��A�(�A���A�x�A�/A��RA��A�`BA��A��RA�S�A�+A�bA�+A�S�A�1'A� �A�p�A���A�7LA��FA���A���A�33A��wA�ĜA�XA��+A�bA��A��RA��A��A���A�x�A��-A�$�A�p�A��/A�Q�A��mA��jA�v�A�ZA�ZA���A���A+A}p�A{��Az�+Au��Ar5?AmG�Ak�PAg`BAe�A`��A\1'A[�AZ��AYdZAW?}AV�AT1'AP��AMdZAK��AI��AG�#AF�DAEt�AC��AC"�A>�A;oA7�wA5A4(�A2�/A1/A.��A-ƨA-hsA,�A,{A)&�A'��A%�#A#�A"v�A!��A!�AG�A��AM�AS�Ar�A�7A�HA5?A��A�A�uAVA(�A`BA�A�#AE�A��A��A-A��A$�AJA|�AK�A��A`BA
�A	x�A�A�RAQ�A|�A/A�/A�A�PA�+A �A �+A J@�E�@�V@�C�@��y@�v�@��@��h@��`@��u@�9X@��@�t�@�5?@��7@�hs@�V@���@�I�@�o@�=q@��@�`B@�9X@��@��y@�!@@�-@��@�G�@�
=@��@�7L@���@��@�@�5?@�j@�F@�l�@�;d@��@�p�@�Ĝ@�Q�@�K�@�ff@�%@��;@�~�@٩�@ؓu@���@ם�@�33@�@ְ!@���@Ձ@ԣ�@���@ӝ�@ӥ�@ӍP@�;d@�~�@�{@ѩ�@�V@мj@�r�@�A�@�A�@�9X@���@�^5@���@�b@˶F@�+@���@ʟ�@ʗ�@Ɂ@ɉ7@ɺ^@ɺ^@ɲ-@ɲ-@�x�@�/@ȼj@�Z@�(�@��;@ǍP@ǥ�@�"�@�33@�l�@�l�@�
=@Ƈ+@�X@ģ�@�  @�1@��
@�t�@°!@�M�@���@���@���@���@�I�@�+@�ff@��H@�33@��@��\@��^@�x�@��@���@��D@�bN@�z�@��@�9X@��@��@���@��@���@�hs@���@�Q�@�1@��w@�|�@���@�\)@��@���@�5?@�M�@���@��u@�r�@�1'@��@��
@��P@�
=@��T@���@���@��T@�$�@�E�@�^5@�$�@��#@�X@�Ĝ@�r�@��@��P@��R@�ff@�p�@���@���@��D@��@��F@�
=@��R@�p�@��@��/@���@��@��`@�Z@��;@��P@�@�~�@�M�@�-@��@���@��h@�G�@�&�@��`@��D@� �@��@�K�@��R@�~�@�V@��@���@�V@�j@�(�@�(�@�1@���@�o@���@��-@�p�@�?}@��@���@���@��@�z�@�1'@��@��w@�|�@�o@���@�E�@��@��^@���@��7@�O�@�V@��9@��@�bN@�I�@�  @��
@��@�|�@�;d@�
=@��@��@���@��\@�M�@��@��#@��@�G�@��@���@��9@�bN@� �@�b@��m@��
@��P@�;d@�@��!@��\@�M�@�{@��@���@��h@�?}@���@�r�@��F@�S�@�;d@�o@��@��!@���@��+@�$�@�@���@���@��7@�/@��@���@�Ĝ@�j@�J@�7L@�%@uO�@h�`@c@ZM�@St�@K33@F{@@bN@8�`@/��@)&�@#��@
=@��@�T@�\@5?@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�K�A�K�A�G�A�G�A�G�A�?}A�;dA�;dA�-A�1'A� �A��A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ĜA�ƨA�ĜA�AܾwAܺ^Aܴ9Aܡ�A�~�A�^5A�A�A�VA��#AۍPA�AجA�p�A�9XA�~�AҋDAϰ!Aˣ�A�VAǡ�A��A�oA�jA��mAć+A�VA��A�(�A���A�x�A�/A��RA��A�`BA��A��RA�S�A�+A�bA�+A�S�A�1'A� �A�p�A���A�7LA��FA���A���A�33A��wA�ĜA�XA��+A�bA��A��RA��A��A���A�x�A��-A�$�A�p�A��/A�Q�A��mA��jA�v�A�ZA�ZA���A���A+A}p�A{��Az�+Au��Ar5?AmG�Ak�PAg`BAe�A`��A\1'A[�AZ��AYdZAW?}AV�AT1'AP��AMdZAK��AI��AG�#AF�DAEt�AC��AC"�A>�A;oA7�wA5A4(�A2�/A1/A.��A-ƨA-hsA,�A,{A)&�A'��A%�#A#�A"v�A!��A!�AG�A��AM�AS�Ar�A�7A�HA5?A��A�A�uAVA(�A`BA�A�#AE�A��A��A-A��A$�AJA|�AK�A��A`BA
�A	x�A�A�RAQ�A|�A/A�/A�A�PA�+A �A �+A J@�E�@�V@�C�@��y@�v�@��@��h@��`@��u@�9X@��@�t�@�5?@��7@�hs@�V@���@�I�@�o@�=q@��@�`B@�9X@��@��y@�!@@�-@��@�G�@�
=@��@�7L@���@��@�@�5?@�j@�F@�l�@�;d@��@�p�@�Ĝ@�Q�@�K�@�ff@�%@��;@�~�@٩�@ؓu@���@ם�@�33@�@ְ!@���@Ձ@ԣ�@���@ӝ�@ӥ�@ӍP@�;d@�~�@�{@ѩ�@�V@мj@�r�@�A�@�A�@�9X@���@�^5@���@�b@˶F@�+@���@ʟ�@ʗ�@Ɂ@ɉ7@ɺ^@ɺ^@ɲ-@ɲ-@�x�@�/@ȼj@�Z@�(�@��;@ǍP@ǥ�@�"�@�33@�l�@�l�@�
=@Ƈ+@�X@ģ�@�  @�1@��
@�t�@°!@�M�@���@���@���@���@�I�@�+@�ff@��H@�33@��@��\@��^@�x�@��@���@��D@�bN@�z�@��@�9X@��@��@���@��@���@�hs@���@�Q�@�1@��w@�|�@���@�\)@��@���@�5?@�M�@���@��u@�r�@�1'@��@��
@��P@�
=@��T@���@���@��T@�$�@�E�@�^5@�$�@��#@�X@�Ĝ@�r�@��@��P@��R@�ff@�p�@���@���@��D@��@��F@�
=@��R@�p�@��@��/@���@��@��`@�Z@��;@��P@�@�~�@�M�@�-@��@���@��h@�G�@�&�@��`@��D@� �@��@�K�@��R@�~�@�V@��@���@�V@�j@�(�@�(�@�1@���@�o@���@��-@�p�@�?}@��@���@���@��@�z�@�1'@��@��w@�|�@�o@���@�E�@��@��^@���@��7@�O�@�V@��9@��@�bN@�I�@�  @��
@��@�|�@�;d@�
=@��@��@���@��\@�M�@��@��#@��@�G�@��@���@��9@�bN@� �@�b@��m@��
@��P@�;d@�@��!@��\@�M�@�{@��@���@��h@�?}@���@�r�@��F@�S�@�;d@�o@��@��!@���@��+@�$�@�@���@���@��7@�/@��@���@�ĜG�O�@�J@�7L@�%@uO�@h�`@c@ZM�@St�@K33@F{@@bN@8�`@/��@)&�@#��@
=@��@�T@�\@5?@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBJBDBDB
=B	7B	7B	7B	7B	7B1B+BDB�B#�B<jBI�BcTB\)BhsB�BB�B  B
=B�B�B�B�B�Bk�B_;BN�BVB�)B�NB��B��B�HB�B�yB�/B��BɺB��B�-B��B�uB�1B�+B�B��B�
BƨB��BffB]/BbNBiyBaHBQ�B�B
��B
�B
�NB
��B
�9B
��B
��B
�hB
�VB
�1B
s�B
ZB
=qB
$�B
uB
DB	��B	�B	��B	�!B	�uB	��B	|�B	y�B	]/B	8RB	B�B	C�B	@�B	49B	,B	�B	JB��B��B�B�mB�NB�5B�#B�
BɺB�}B�^B�XB�?B�9B�B��B��B��B�B�B�'B�B��B��B��B��B��B��B�{B�bB�PB�7B�%B�B�B�7B�7B�7B�=B�7B�\B�oB�bB�bB�bB�PB�bB�uB��B�RB�}B��BĜBȴB��B��B��B�B�B�#B�B�#B�/B�)B�/B��B��B��B��B��B��B�B�B�)B�/B�;B�BB�HB�HB�NB�`B�sB�sB�yB�yB�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	B	%B	1B		7B	JB	oB	�B	�B	�B	�B	�B	�B	{B	{B	uB	�B	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	)�B	-B	.B	.B	/B	0!B	5?B	=qB	B�B	F�B	H�B	K�B	L�B	L�B	O�B	O�B	R�B	S�B	T�B	S�B	S�B	R�B	VB	ZB	\)B	^5B	aHB	aHB	aHB	bNB	bNB	bNB	cTB	dZB	cTB	bNB	aHB	ffB	hsB	p�B	u�B	x�B	y�B	{�B	{�B	z�B	|�B	�B	�B	�B	�%B	�%B	�%B	�7B	�DB	�DB	�=B	�+B	�+B	�JB	�oB	�uB	�uB	�oB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�-B	�'B	�'B	�-B	�3B	�3B	�LB	�dB	�qB	�wB	�}B	��B	B	B	��B	B	ÖB	ĜB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�;B	�;B	�;B	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�mB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
�B
�B
#�B
$�B
,B
1'B
6FB
<jB
C�B
H�B
O�B
XB
[#B
`BB
dZB
jB
m�B
p�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B?B?B@B>BAB@B?B@B@B?B@BAB?B@B@B@BABABAB@BAB@B@BBBBB@BBBABAB@B@BABAB8B;B
1B	-B	0B	.B	,B	*B(B!B9BqB#�B<^BI�BcGB\BhiB�7B�B��B
.B�B�B�B�B�BkwB_-BN�BFB�B�?B��B��B�:B�}B�kB�!B��BɮB�vB�B��B�fB�!B�B�B��B��BƖB��BfWB]Bb@BiiBa8BQ�B�B
��B
�B
�AB
��B
�.B
��B
�tB
�_B
�NB
�$B
s�B
ZB
=jB
$�B
lB
>B	��B	�B	��B	�B	�oB	��B	|�B	y�B	]-B	8NB	B�B	C�B	@�B	46B	,B	�B	HB��B��B�B�nB�QB�6B�$B�BɻB�~B�`B�XB�AB�<B�B��B��B��B�B�	B�)B�B��B��B��B��B��B��B�~B�eB�RB�;B�(B�!B�#B�<B�;B�;B�@B�8B�^B�pB�fB�gB�dB�TB�cB�uB��B�RB�{B��BĜBȴB��B��B��B�B�B� B�B� B�,B�&B�,B��B��B��B��B��B��B�B�B�(B�,B�8B�>B�CB�FB�LB�\B�rB�pB�wB�xB�zB�B�B�B�B�B�B��B��B��B��B��B��B��B	B	B	B	B	 B	,B		4B	DB	hB	~B	�B	�B	�B	�B	�B	xB	vB	pB	yB	�B	�B	�B	�B	�B	�B	�B	#�B	$�B	)�B	-B	.B	.B	/B	0B	59B	=iB	B�B	F�B	H�B	K�B	L�B	L�B	O�B	O�B	R�B	S�B	T�B	S�B	S�B	R�B	U�B	ZB	\"B	^,B	a=B	a@B	a@B	bEB	bHB	bEB	cLB	dSB	cLB	bDB	aBB	f^B	hlB	p�B	u�B	x�B	y�B	{�B	{�B	z�B	|�B	��B	�B	�B	�B	�B	�B	�,B	�9B	�:B	�4B	�B	�"B	�@B	�cB	�lB	�lB	�cB	�_B	�eB	�kB	�rB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�B	�B	�B	�'B	�&B	�>B	�XB	�dB	�jB	�qB	�wB	B	B	�}B	B	ÈB	đB	ÊB	ďB	ŕB	ŔB	ƜB	ǣB	ǣB	ȧB	ɮB	ʴB	˺B	̿B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�!B	�#B	�!B	�*B	�(B	�+B	�,B	�-B	�-B	�9B	�@B	�@B	�AB	�GB	�MB	�MB	�cB	�`B	�aB	�fB	�fB	�dB	�jB	�lB	�nB	�rB	�xB	�qB	�xB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
B
B
B
B
B
B
B

B
B

G�O�B
"B
xB
�B
#�B
$�B
+�B
1B
67B
<ZB
C�B
H�B
O�B
X B
[B
`3B
dIB
jnB
m�B
p�B
t�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.16 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436272016080714362720160807143627  AO  ARCAADJP                                                                    20150226221320    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221320  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221320  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143627  IP                  G�O�G�O�G�O�                