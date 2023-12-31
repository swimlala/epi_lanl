CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:58:27Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qt   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �$   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20181121125827  20190408133245  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5286                            2C  D   APEX                            6531                            072314                          846 @����RB1   @��s���@5-O�;dZ�b�9XbN1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#�fD$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D2��D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dtl�DyY�D��D�FfD�|�D�VfD��D�L�D��3D��3D��D�C3D�l�D�ٚD� D�0 D�ffD��3D���D�<�D� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�AA!AAAaA��HA��HA��HA��HA��HA��HA��HA��HB p�Bp�Bp�Bp�B p�B(p�B0p�B8p�B@p�BH�
BPp�BXp�B`p�Bhp�Bpp�Bxp�B�8RB�8RB�8RB�8RB�8RB�8RB�k�B�8RB�B�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RB�8RC )C)C)C)C)C
)C)C)C)C)C)C)C)C)C5�C)C )C")C$)C&)C()C*)C,)C.)C0)C2)C4)C6)C8)C:)C<)C>)C@)CB)CD)CF)CH)CJ)CL)CN)CP)CR)CT)CV)CX)CZ)C\)C^)C`)Cb)Cd)Cf)Ch)Cj5�Cl)Cn)Cp)Cr)Ct)Cv)Cx)Cz)C|)C~)C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D 
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
D�
D
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
D#�pD$
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
D3 �D3�
D4
D4�
D5
D5�
D6
D6�
D7
D7�
D8
D8��D9
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
DA
DA�
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
DL
DL�
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
DV
DV�
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
Db
Db�
Dc
Dc�
Dd
Dd�
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
Dk
Dk�
Dl
Dl�
Dm
Dm�
Dn
Dn�
Do
Do�
Dp
Dp�
Dq
Dq�
Dr
Dr�
Ds
Ds�
Dt
Dts�Dy`�D�D�I�D��RD�Y�D�D�PRD���D���D�D�F�D�pRD��D��D�3�D�i�D�ƸD� RD�@RD�D��R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�7LA�5?A�9XA�-A�bA�A��A�ĜAӝ�Aӏ\Aӏ\A�~�A�hsA�\)A�VA�C�A��AҮA҉7A�x�A�E�AѾwA�v�A�/A�JA�l�A���A��AͅA�ZA�9XA�`BA�"�Aš�Aú^A��A�n�A�A��-A�
=A�1'A�O�A���A�E�A�"�A�^5A�"�A�"�A��jA���A�33A��yA�A�I�A�bNA��A��
A��mA��A���A��RA�v�A�1A��!A���A�dZA�\)A�1'A�jA�C�A�A��wA�VA�(�A�1A��#A��mA�7LA�VA��A���A��A��A��-A���A���A���A�A�A��HA�{A�ffA�"�A�"�A���A���A��9A�+A�ȴA�%A�;dA�r�A���A�=qA�ĜA�{A�z�A~�HA|(�A{�#Az�/AyƨAx��Aw
=As�mAq7LAo�Al=qAjVAh�Af��AdȴAc�Aa?}A_&�A^A[�7AW��AU�AT�AQ��AP(�AL�DAK�AH��AG�AF��AE�^AD9XAA�-A@1'A>Q�A<��A:1'A8I�A7��A6��A69XA5�hA5`BA4�\A2=qA0=qA.ZA-/A,��A,�A+%A)�A(��A(  A&��A&E�A%��A$��A#S�A"=qA!��A!�A ffA��A�RA�mAE�A&�A�!A��A%Av�A|�A�An�A��AM�A�A��A�AVAVAM�A�PA9XA
�uA
ffA	��A	XA$�AXAp�AdZA �A7LAjA�AE�Ax�A �u@�dZ@���@�G�@�z�@�(�@���@�t�@��^@�Ĝ@�33@�@�ȴ@�@�G�@�@@��T@�h@��@���@�C�@��@�$�@�dZ@�^5@�7L@�1@���@���@��@��@�"�@�^5@��@� �@�o@��@�=q@�hs@ܬ@�(�@�l�@ڏ\@���@�9X@�  @�b@���@�33@�@�^5@�Ĝ@Ӿw@�"�@ҏ\@љ�@д9@϶F@�@�v�@��T@ͩ�@���@̴9@��
@˥�@�K�@ʸR@�n�@�^5@�v�@�$�@�@�$�@���@Ɂ@�V@ǝ�@Ɵ�@�V@��#@�?}@��`@ļj@ċD@�Q�@�9X@��;@�33@�~�@�=q@�J@�x�@�?}@�1'@�S�@�@��@�"�@�+@�;d@�\)@�\)@�C�@�"�@�V@��@���@�/@�%@���@�Z@��@���@��@���@�\)@�t�@�{@��7@���@�ƨ@�|�@�|�@�o@�{@�x�@�/@��@�Q�@�;d@�
=@���@���@�v�@�V@�$�@��^@�p�@�?}@�X@�`B@�X@�%@��9@�bN@��@�K�@��+@�$�@���@���@���@�%@�A�@��@�b@��@�S�@�ȴ@�M�@�$�@�@���@�X@�V@��`@���@��@�dZ@�dZ@�l�@�S�@�S�@�33@��@��@���@�{@���@��7@��@��@�I�@�b@���@�;d@��H@��@���@�@�x�@�?}@�G�@���@�(�@��F@���@�C�@�~�@�@���@�/@��@� �@��@��@�t�@�l�@�t�@�
=@��@��!@�v�@���@���@�r�@��@��@��@�ff@���@�G�@��@��j@�z�@�j@�Q�@�(�@�1@���@�  @�ƨ@�t�@�\)@�33@���@��@�ȴ@���@���@��@�`B@�O�@�/@�%@���@��j@���@��@�bN@�b@��m@���@���@�l�@�\)@�33@�o@��y@�ȴ@��!@���@�V@�5?@��T@���@�O�@���@�Ĝ@��u@�bN@�A�@��@��w@��@��@���@�t�@�S�@��@���@�ƨ@~E�@u�-@ix�@cC�@\�j@U�@M�-@E�-@@1'@:J@2~�@-V@&�@"-@�@&�@�@�9@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�7LA�5?A�9XA�-A�bA�A��A�ĜAӝ�Aӏ\Aӏ\A�~�A�hsA�\)A�VA�C�A��AҮA҉7A�x�A�E�AѾwA�v�A�/A�JA�l�A���A��AͅA�ZA�9XA�`BA�"�Aš�Aú^A��A�n�A�A��-A�
=A�1'A�O�A���A�E�A�"�A�^5A�"�A�"�A��jA���A�33A��yA�A�I�A�bNA��A��
A��mA��A���A��RA�v�A�1A��!A���A�dZA�\)A�1'A�jA�C�A�A��wA�VA�(�A�1A��#A��mA�7LA�VA��A���A��A��A��-A���A���A���A�A�A��HA�{A�ffA�"�A�"�A���A���A��9A�+A�ȴA�%A�;dA�r�A���A�=qA�ĜA�{A�z�A~�HA|(�A{�#Az�/AyƨAx��Aw
=As�mAq7LAo�Al=qAjVAh�Af��AdȴAc�Aa?}A_&�A^A[�7AW��AU�AT�AQ��AP(�AL�DAK�AH��AG�AF��AE�^AD9XAA�-A@1'A>Q�A<��A:1'A8I�A7��A6��A69XA5�hA5`BA4�\A2=qA0=qA.ZA-/A,��A,�A+%A)�A(��A(  A&��A&E�A%��A$��A#S�A"=qA!��A!�A ffA��A�RA�mAE�A&�A�!A��A%Av�A|�A�An�A��AM�A�A��A�AVAVAM�A�PA9XA
�uA
ffA	��A	XA$�AXAp�AdZA �A7LAjA�AE�Ax�A �u@�dZ@���@�G�@�z�@�(�@���@�t�@��^@�Ĝ@�33@�@�ȴ@�@�G�@�@@��T@�h@��@���@�C�@��@�$�@�dZ@�^5@�7L@�1@���@���@��@��@�"�@�^5@��@� �@�o@��@�=q@�hs@ܬ@�(�@�l�@ڏ\@���@�9X@�  @�b@���@�33@�@�^5@�Ĝ@Ӿw@�"�@ҏ\@љ�@д9@϶F@�@�v�@��T@ͩ�@���@̴9@��
@˥�@�K�@ʸR@�n�@�^5@�v�@�$�@�@�$�@���@Ɂ@�V@ǝ�@Ɵ�@�V@��#@�?}@��`@ļj@ċD@�Q�@�9X@��;@�33@�~�@�=q@�J@�x�@�?}@�1'@�S�@�@��@�"�@�+@�;d@�\)@�\)@�C�@�"�@�V@��@���@�/@�%@���@�Z@��@���@��@���@�\)@�t�@�{@��7@���@�ƨ@�|�@�|�@�o@�{@�x�@�/@��@�Q�@�;d@�
=@���@���@�v�@�V@�$�@��^@�p�@�?}@�X@�`B@�X@�%@��9@�bN@��@�K�@��+@�$�@���@���@���@�%@�A�@��@�b@��@�S�@�ȴ@�M�@�$�@�@���@�X@�V@��`@���@��@�dZ@�dZ@�l�@�S�@�S�@�33@��@��@���@�{@���@��7@��@��@�I�@�b@���@�;d@��H@��@���@�@�x�@�?}@�G�@���@�(�@��F@���@�C�@�~�@�@���@�/@��@� �@��@��@�t�@�l�@�t�@�
=@��@��!@�v�@���@���@�r�@��@��@��@�ff@���@�G�@��@��j@�z�@�j@�Q�@�(�@�1@���@�  @�ƨ@�t�@�\)@�33@���@��@�ȴ@���@���@��@�`B@�O�@�/@�%@���@��j@���@��@�bN@�b@��m@���@���@�l�@�\)@�33@�o@��y@�ȴ@��!@���@�V@�5?@��T@���@�O�@���@�Ĝ@��u@�bN@�A�@��@��w@��@��@���@�t�@�S�@��@���@�ƨ@~E�@u�-@ix�@cC�@\�j@U�@M�-@E�-@@1'@:J@2~�@-V@&�@"-@�@&�@�@�9@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
hsB
hsB
hsB
hsB
hsB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
iyB
z�B
��B
��B
��B
�BB
�fB
��B
��BVB#�B6FB0!B,B\B
��B
��B
�B
��BBDBuB%�BE�Be`B�JB�-BŢB��B�}B��B��B��B�LBBŢB��B��B��B�/BVB>wBP�BQ�BR�B|�B�B~�Bn�BVBD�BE�B@�B;dB>wBR�B�JB�LB��B��B�LB��Bv�Be`BQ�BB�B33B.B"�B%B�HBB��B�uB�JB� BZB'�B
��B
��B
�=B
aHB
YB
VB
L�B
?}B
�B
B
B
B
B	�ZB	��B	�B	��B	�B	�fB	�B	ŢB	�9B	��B	�oB	�B	t�B	jB	^5B	P�B	F�B	:^B	0!B	�B	PB	B��B�yB�BB��B��BĜB��B�qB�jB�RB�B�B�!B�9B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB��B��B��B�hB�oB��B�uB�uB�oB�bB�DB�B� B�B�1B�+B�1B�%B�B� B{�Bu�Bn�BffBe`BcTB^5B]/BZBYBXBW
BVBT�BR�BS�B`BBp�Bo�Bo�Bl�BhsBe`Be`BdZBaHBaHBbNBe`Bm�Bt�Bs�Bp�Bs�Bp�BhsBgmBl�Bo�Bt�Bx�Bw�Bz�B|�Bz�B{�B�B�B}�B~�B}�B{�B�+B�VB�uB��B��B�{B��B��B��B��B��B�B�!B�-B�-B�dB�dB��BŢB��B��B��B�#B�#B�B�B�B�B�#B�5B�NB�mB�B�B�B��B��B��B��B��B	B	%B	1B	\B	�B	�B	 �B	#�B	"�B	"�B	�B	�B	"�B	%�B	(�B	+B	-B	0!B	2-B	49B	6FB	9XB	<jB	>wB	?}B	D�B	F�B	H�B	K�B	N�B	R�B	S�B	S�B	T�B	VB	ZB	[#B	[#B	\)B	]/B	\)B	]/B	aHB	aHB	bNB	e`B	ffB	ffB	hsB	l�B	p�B	p�B	q�B	t�B	u�B	w�B	x�B	z�B	z�B	z�B	|�B	}�B	~�B	�B	�B	�B	�B	�B	�%B	�+B	�1B	�=B	�JB	�\B	�\B	�bB	�hB	�hB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�FB	�LB	�LB	�LB	�^B	�jB	�jB	�dB	�dB	�jB	�jB	�jB	�dB	�^B	�XB	�XB	��B	��B	��B	��B	ÖB	ÖB	ÖB	ÖB	B	��B	�}B	�}B	��B	�}B	�}B	��B	��B	��B	��B	��B	��B	B	B	ĜB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�#B	�#B	�)B	�)B	�/B	�/B	�5B	�BB	�BB	�BB	�HB	�NB	�ZB	�ZB	�`B	�fB	�fB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
1B
PB
{B
�B
"�B
&�B
.B
49B
>wB
D�B
I�B
L�B
R�B
YB
]/B
cTB
gmB
m�B
p�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
hkB
hlB
hhB
hlB
hkB
gfB
gfB
geB
gfB
gfB
gfB
gcB
gcB
gdB
f\B
gdB
iqB
z�B
��B
�B
˿B
�:B
�]B
��B
��BJB#�B6>B0B, BRB
��B
��B
�B
��B �B:BmB%�BE�BeWB�?B�$BśB��B�tB��B��B��B�DBBŗB��B��B��B�&BNB>lBP�BQ�BR�B|�B��B~�Bn�BU�BD�BE�B@wB;[B>nBR�B�<B�BB��BʷB�DB��Bv�BeSBQ�BB�B3(B.B"�BB�:BB��B�hB�AB�BZB'�B
��B
��B
�4B
a;B
YB
U�B
L�B
?tB
�B
B
B
 �B
 �B	�NB	��B	�B	��B	�B	�YB	�B	ŖB	�.B	��B	�cB	�B	t�B	jsB	^'B	P�B	F�B	:TB	0B	�B	BB	 �B��B�mB�5B��B˻BđB�vB�eB�_B�EB�B�B�B�-B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�zB�hB�B��B�rB�]B�`B�sB�hB�hB�eB�WB�7B�B�B�B�"B�!B�"B�B�B�B{�Bu�Bn�BfYBeRBcGB^)B]"BZBY
BXBV�BU�BT�BR�BS�B`6Bp�Bo�Bo�Bl~BheBeSBeUBdLBa9Ba9BbBBeTBm�Bt�Bs�Bp�Bs�Bp�BhfBg_BlBo�Bt�Bx�Bw�Bz�B|�Bz�B{�B��B��B}�B~�B}�B{�B�B�HB�iB�|B�uB�oB�zB�uB�B��B��B��B�B� B�!B�XB�VB�BŕB˻B��B��B�B�B�B�B�B�B�B�%B�?B�^B�qB�B�B��B��B��B��B��B	B	B	%B	MB	yB	�B	 �B	#�B	"�B	"�B	�B	�B	"�B	%�B	(�B	*�B	-B	0B	2B	4+B	69B	9IB	<]B	>jB	?pB	D�B	F�B	H�B	K�B	N�B	R�B	S�B	S�B	T�B	U�B	ZB	[B	[B	\B	]!B	\B	]"B	a8B	a8B	b@B	eRB	fZB	fXB	heB	l|B	p�B	p�B	q�B	t�B	u�B	w�B	x�B	z�B	z�B	z�B	|�B	}�B	~�B	��B	�B	�B	�B	�B	�B	�B	�#B	�-B	�;B	�LB	�NB	�SB	�[B	�\B	�_B	�pB	�nB	�tB	�tB	�xB	�wB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	��B	�B	�B	�B	�#B	�9B	�=B	�=B	�>B	�OB	�]B	�[B	�UB	�WB	�\B	�\B	�\B	�UB	�PB	�IB	�KB	�vB	�vB	�vB	�wB	ÇB	ÆB	ÆB	ÇB	B	�wB	�nB	�mB	�vB	�nB	�mB	�vB	�uB	�sB	�uB	�}B	�yB	B	B	ďB	čB	ŕB	œB	ƙB	ƛB	ǟB	ǞB	ȣB	ȦB	ɪB	ʴB	˸B	̿B	̿B	̿B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�B	�B	� B	� B	�&B	�5B	�3B	�4B	�9B	�@B	�MB	�MB	�OB	�XB	�VB	�]B	�eB	�iB	�kB	�oB	�wB	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
#B
BB
oB
�B
"�B
&�B
.B
4+B
>gB
D�B
I�B
L�B
R�B
YB
]B
cGB
g]B
m�B
p�B
u�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.11 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904081332452019040813324520190408133245  AO  ARCAADJP                                                                    20181121125827    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125827  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125827  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190408133245  IP                  G�O�G�O�G�O�                