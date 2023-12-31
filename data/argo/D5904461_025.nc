CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:47Z AOML 3.0 creation; 2016-08-07T21:36:31Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221347  20160807143631  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_025                   2C  D   APEX                            6531                            072314                          846 @�1��|��1   @�1�'��@1�&�x���d�x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B���B�  B�  B�  B�ffB�  B�  B�  B�  B���B�  B�  B�  B�  C   C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@�CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy� D�#3D�9�D�|�D���D��D�Y�D�|�D���D�fD�I�D�|�DǼ�DͦfD�VfDڀ D��fD�fD�<�D�|�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBB\)B B(B0B8B@BHBPBXB`BhBpBxB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{B�aHB�aHB�.B�aHB�aHB�aHB�ǮB�aHB�aHB�aHB�aHB�.B�aHB�aHB�aHB�aHC 0�C0�C
C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@J>CBJ>CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$��D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)DtҏDy�)D�)GD�?�D���D���D�"�D�_�D���D���D�zD�O�D���D���DͬzD�\zDچD��zD�zD�B�D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ZA�E�A�?}A�=qA�=qA�?}A�E�A�C�A�E�A�K�A�S�A�VA�ZA�\)A�ffA�l�A�r�A�v�A�~�A·+AΏ\AΙ�AάAΰ!AήAήAΩ�AΩ�AΡ�AΛ�AΑhA΍PA΍PAΕ�AΡ�Aΰ!A�ĜA��HA�=qA�Q�A�M�A�?}A�oA���A���A��A��A��
A�v�A�C�A�9XA� �A��A���A���A�ƨA��7A�Q�A�
=A���A��HA���A���A�ĜA���A���A��TA�hsA�
=A�hsA��-A���A�33A�-A��7A�E�A�z�A��A�oA�A�VA�hsA��A�`BA�1'A��;A��A��A� �A�%A��^A~�!A|-Aw�PArQ�Ap��AmXAhz�AbM�A` �A^bNA[?}AV�AT�`ASK�AQ�
AMK�AH��AE�AD1'ABVAA��AA\)A?�mA=�A<r�A;G�A:^5A9A85?A7�A733A5�hA3�7A2�9A2�A1��A/p�A,�A,1A+l�A+VA*�RA)S�A'��A&�/A&^5A&JA%33A$�/A$A"�RA"�+A �A��A�A�^A�/Ar�A�#A7LA^5Al�A1A��AQ�A�mA��A%A��AffA�FA�A=qA��A�AA
��A
�A
5?A	��A �AC�AAn�At�A��A�A�mA�AhsAK�A�RA��A n�@�C�@�v�@�hs@��D@�dZ@���@��@���@�~�@���@�A�@��@���@�bN@��@�v�@홚@�9X@�|�@�ȴ@�@�r�@�S�@���@���@�/@�I�@�S�@�-@�J@��@�O�@�V@��@�b@�o@�X@ܼj@�A�@ە�@��@�M�@���@�O�@���@�Q�@�;d@�33@�+@ָR@�=q@�@���@ա�@�/@��@�"�@���@҇+@��T@�p�@���@Ь@�r�@��@��y@Ώ\@�V@��@�hs@���@�b@�l�@��@ʧ�@��@�=q@�J@��@��T@ɉ7@���@ț�@ț�@�(�@��;@�K�@Ƨ�@�-@�@���@ř�@Ł@�7L@�Ĝ@�z�@Ý�@���@�I�@���@Õ�@Å@��@�+@�ff@�^5@�o@�|�@�+@��^@�Ĝ@��@��u@�V@�5?@�5?@��@�;d@�|�@å�@�t�@�l�@�\)@��@\@�V@�=q@��@���@���@�V@���@��u@��P@��@�~�@�=q@���@���@��7@�x�@�hs@�7L@��@�  @��P@�o@��@��-@�?}@�z�@�l�@�@��@�-@�X@��`@�Q�@��@��@��
@��F@�l�@��@��\@�J@��-@�O�@��j@��@��u@�z�@�j@��@��;@���@�33@��@�-@��@��-@��D@��@�
=@��\@��h@���@��/@��/@��/@��@�%@���@���@���@���@��@�  @�C�@��H@��+@�-@�5?@��@��T@�J@�-@�@���@���@�`B@��/@���@�r�@�1'@�ƨ@���@�t�@�;d@���@���@���@�v�@�n�@�-@���@���@��@�p�@�G�@���@��u@�(�@��F@�K�@��y@�V@�J@��@�@�x�@���@���@�Z@��m@���@�\)@���@��!@��@��@���@�n�@�J@�O�@�`B@�G�@�%@��9@�bN@��m@��@�+@�o@��!@���@�G�@��@��u@�Z@�A�@� �@���@�33@�@��@��@��+@�@���@�p�@��@�?}@��j@�bN@��w@���@�|�@�"�@��R@�V@��@��@��-@���@�p�@��`@��`@��/@���@�(�@���@�|�@��y@�7L@��@~@v5?@n{@fE�@\�@R~�@K�F@C�
@>��@8��@1x�@+�m@#t�@?}@K�@�@5?@
^5@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�ZA�E�A�?}A�=qA�=qA�?}A�E�A�C�A�E�A�K�A�S�A�VA�ZA�\)A�ffA�l�A�r�A�v�A�~�A·+AΏ\AΙ�AάAΰ!AήAήAΩ�AΩ�AΡ�AΛ�AΑhA΍PA΍PAΕ�AΡ�Aΰ!A�ĜA��HA�=qA�Q�A�M�A�?}A�oA���A���A��A��A��
A�v�A�C�A�9XA� �A��A���A���A�ƨA��7A�Q�A�
=A���A��HA���A���A�ĜA���A���A��TA�hsA�
=A�hsA��-A���A�33A�-A��7A�E�A�z�A��A�oA�A�VA�hsA��A�`BA�1'A��;A��A��A� �A�%A��^A~�!A|-Aw�PArQ�Ap��AmXAhz�AbM�A` �A^bNA[?}AV�AT�`ASK�AQ�
AMK�AH��AE�AD1'ABVAA��AA\)A?�mA=�A<r�A;G�A:^5A9A85?A7�A733A5�hA3�7A2�9A2�A1��A/p�A,�A,1A+l�A+VA*�RA)S�A'��A&�/A&^5A&JA%33A$�/A$A"�RA"�+A �A��A�A�^A�/Ar�A�#A7LA^5Al�A1A��AQ�A�mA��A%A��AffA�FA�A=qA��A�AA
��A
�A
5?A	��A �AC�AAn�At�A��A�A�mA�AhsAK�A�RA��A n�@�C�@�v�@�hs@��D@�dZ@���@��@���@�~�@���@�A�@��@���@�bN@��@�v�@홚@�9X@�|�@�ȴ@�@�r�@�S�@���@���@�/@�I�@�S�@�-@�J@��@�O�@�V@��@�b@�o@�X@ܼj@�A�@ە�@��@�M�@���@�O�@���@�Q�@�;d@�33@�+@ָR@�=q@�@���@ա�@�/@��@�"�@���@҇+@��T@�p�@���@Ь@�r�@��@��y@Ώ\@�V@��@�hs@���@�b@�l�@��@ʧ�@��@�=q@�J@��@��T@ɉ7@���@ț�@ț�@�(�@��;@�K�@Ƨ�@�-@�@���@ř�@Ł@�7L@�Ĝ@�z�@Ý�@���@�I�@���@Õ�@Å@��@�+@�ff@�^5@�o@�|�@�+@��^@�Ĝ@��@��u@�V@�5?@�5?@��@�;d@�|�@å�@�t�@�l�@�\)@��@\@�V@�=q@��@���@���@�V@���@��u@��P@��@�~�@�=q@���@���@��7@�x�@�hs@�7L@��@�  @��P@�o@��@��-@�?}@�z�@�l�@�@��@�-@�X@��`@�Q�@��@��@��
@��F@�l�@��@��\@�J@��-@�O�@��j@��@��u@�z�@�j@��@��;@���@�33@��@�-@��@��-@��D@��@�
=@��\@��h@���@��/@��/@��/@��@�%@���@���@���@���@��@�  @�C�@��H@��+@�-@�5?@��@��T@�J@�-@�@���@���@�`B@��/@���@�r�@�1'@�ƨ@���@�t�@�;d@���@���@���@�v�@�n�@�-@���@���@��@�p�@�G�@���@��u@�(�@��F@�K�@��y@�V@�J@��@�@�x�@���@���@�Z@��m@���@�\)@���@��!@��@��@���@�n�@�J@�O�@�`B@�G�@�%@��9@�bN@��m@��@�+@�o@��!@���@�G�@��@��u@�Z@�A�@� �@���@�33@�@��@��@��+@�@���@�p�@��@�?}@��j@�bN@��w@���@�|�@�"�@��R@�V@��@��@��-@���@�p�@��`@��`@��/@���@�(�@���@�|�G�O�@�7L@��@~@v5?@n{@fE�@\�@R~�@K�F@C�
@>��@8��@1x�@+�m@#t�@?}@K�@�@5?@
^5@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�XB	�^B	�dB	�jB	�qB	�wB	��B	B	ÖB	ŢB	ȴB	ȴB	��B	��B	��B	��B	�B	�
B	�/B	�HB	�fB	�B	��B	��B
B
B
1B
JB
\B
oB
{B
�B
�B
�B
"�B
&�B
.B
9XB
r�B
�hB
��B
��B
��B
��B
��B
��B
��B
��B
�LB"�B8RBA�BXB_;BgmBhsBe`BcTBm�Bk�Bk�Bk�BjBgmBXBK�B<jB+B
�/B
�B
��B
��B
ƨB
ŢB
��B
ǮB
��B
��B
��B
��B
�qB
�'B
��B
�JB
hsB
R�B
:^B
1'B
'�B

=B	�B	�)B	ȴB	��B	�B	z�B	hsB	H�B	2-B	$�B	�B		7B��B�B�ZB�BȴB�qB�^B�9B�B�B��B�'B�?B�?B�XB�XB�FB�?B�?B�9B�3B�-B�'B�B�B�!B�LB�RB�RB�RB�LB�XB�RB�FB�FB�?B�?B�RB�RB�^B�wB�wB�qB�dB�^B�dBŢBƨB��B��B��B��B��B��B��B��B��B��B��B��BŢBĜBÖB�}B�LB�FB�FB�?B�9B�!B�B��B��B��B�B�!B�-B�9B�FB�XB�}B�wB�^B�wB�}B�}B�qB�qB�qB�wB�}B��BBÖBÖBƨBɺB��B��B��B��B�B�B�B�/B�;B�HB�ZB�`B�ZB�BB�ZB�mB�yB�B�B�B�B�B��B��B��B��B��B	B	1B	JB	JB	VB	uB	{B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	%�B	$�B	&�B	(�B	-B	0!B	1'B	:^B	<jB	<jB	=qB	=qB	>wB	?}B	A�B	C�B	E�B	H�B	M�B	O�B	S�B	T�B	VB	XB	ZB	[#B	]/B	]/B	^5B	_;B	aHB	aHB	dZB	ffB	gmB	hsB	jB	jB	iyB	m�B	r�B	s�B	t�B	x�B	y�B	}�B	}�B	� B	�1B	�PB	�JB	�+B	�%B	�%B	�1B	��B	��B	��B	��B	��B	��B	�B	�!B	�!B	�!B	�!B	�'B	�-B	�-B	�-B	�-B	�3B	�9B	�9B	�9B	�FB	�FB	�LB	�RB	�dB	�jB	�jB	�jB	�qB	�qB	�wB	�}B	��B	��B	ÖB	ÖB	ĜB	ƨB	ȴB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�B	�
B	�B	��B	��B	��B	��B	��B	��B	�B	�B	�/B	�/B	�5B	�5B	�5B	�BB	�;B	�;B	�;B	�5B	�BB	�NB	�TB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
B
B
  B
  B	��B
B
B
B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
B
%B
+B
+B
1B
1B
1B
�B
JB
{B
�B
�B
&�B
-B
33B
:^B
=qB
D�B
H�B
M�B
R�B
YB
`BB
e`B
k�B
p�B
t�B
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	�SB	�\B	�aB	�dB	�kB	�uB	��B	B	ÐB	ŞB	ȭB	ȮB	ʾB	��B	��B	��B	��B	�B	�-B	�@B	�`B	�B	��B	��B
B
B
/B
DB
UB
gB
xB
xB
�B
�B
"�B
&�B
.B
9OB
r�B
�\B
��B
��B
��B
��B
��B
��B
��B
��B
�?B"�B8DBA|BXB_,Bg_BhdBeRBcEBm�BkuBkuBktBjpBg_BX BK�B<\BB
�B
��B
��B
��B
ƗB
ŖB
�sB
ǠB
ʳB
ʴB
˺B
̽B
�aB
�B
��B
�<B
heB
R�B
:QB
1B
'�B

1B	�B	�!B	ȬB	��B	�B	z�B	hnB	H�B	2&B	$�B	�B		5B��B�B�WB�BȲB�pB�_B�8B�B�
B��B�&B�?B�?B�VB�XB�GB�?B�?B�9B�0B�,B�&B�B�	B�B�JB�OB�OB�OB�GB�TB�OB�CB�EB�<B�>B�NB�PB�\B�qB�rB�nB�`B�]B�aBŝBƤB��B��B��B��B��B��B��B��B��B��B��B��BŝBĘBÐB�yB�FB�EB�BB�;B�4B�B�B��B��B��B�B�B�)B�5B�AB�SB�wB�rB�ZB�sB�xB�vB�kB�lB�kB�sB�xB�}BBÏBÑBƢBɵBʼB��B��B��B��B�B�B�'B�4B�AB�TB�XB�TB�>B�RB�dB�pB�B�B�B�B�B��B��B��B��B��B	B	'B	?B	@B	NB	jB	rB	vB	~B	�B	�B	�B	�B	�B	�B	!�B	"�B	%�B	$�B	&�B	(�B	-B	0B	1B	:TB	<^B	<_B	=fB	=fB	>jB	?rB	A}B	C�B	E�B	H�B	M�B	O�B	S�B	T�B	U�B	XB	ZB	[B	]$B	]!B	^*B	_.B	a=B	a<B	dMB	fZB	gaB	hhB	jqB	jsB	ilB	m�B	r�B	s�B	t�B	x�B	y�B	}�B	}�B	�B	�%B	�AB	�<B	�B	�B	�B	�"B	�{B	�|B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�&B	�*B	�(B	�,B	�7B	�8B	�=B	�BB	�TB	�YB	�[B	�YB	�cB	�aB	�iB	�lB	�rB	�wB	ÅB	ÇB	ĊB	ƙB	ȢB	ȤB	ȥB	ɫB	˴B	˵B	̻B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�"B	�#B	�$B	�0B	�*B	�*B	�'B	�%B	�1B	�=B	�BB	�]B	�fB	�iB	�nB	�nB	�rB	�zB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B	��B
�B
B
B
B
B
B
 B
	B
B
B
�B
�B
 �B	��B	��B
 �B
 �B	��B	��B	��B
 �B
 �B
 �B	��B	��B	��B
�B
B
�B
�B
 �B
 �B
�B
�B
�B
 B
�B
	B
B
B
B
B
B
B
B
B
B
!G�O�B
8B
iB
�B
�B
&�B
,�B
3!B
:KB
=\B
D�B
H�B
M�B
R�B
Y B
`,B
eMB
kqB
p�B
t�B
w�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.19 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436312016080714363120160807143631  AO  ARCAADJP                                                                    20150226221347    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221347  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221347  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143631  IP                  G�O�G�O�G�O�                