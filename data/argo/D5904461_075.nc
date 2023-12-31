CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-10-02T19:20:54Z AOML 3.0 creation; 2016-08-07T21:36:39Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151002192054  20160807143639  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               KA   AO  5286_8897_075                   2C  D   APEX                            6531                            072314                          846 @�s�	>.1   @�s� s@2� ě���cC��v�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    KA   B   B   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy� D��D�@ D�|�D��3D�3D�9�D���D���D��D�I�D�Y�D�ٚD�	�D�FfDڃ3D��fD���D�9�D�vfD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @L(�@�{@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB B(B0B8B@BHBPBY(�B`BhBpBxB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{B�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL0�CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�ClJ>CnJ>Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC��C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM�DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt��Dy�)D�"�D�FD���D��GD�GD�?�D���D���D��D�O�D�_�D�߮D��D�LzDډGD��zD��D�?�D�|zD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�&�A�&�A�&�A�$�A�&�A�&�A�+A�$�A�&�A�&�A�-A�-A�+A�+A�-A�-A�(�A�(�A�+A�33A�33A�/A�7LA�9XA�;dA�;dA�;dA�=qA�=qA�=qA�?}A�=qA�?}A�7LA�ȴA�|�A��AЏ\A���A��#A�t�A�ȴA�K�A�+A�1'A�^5A�^5AŲ-Aħ�A��
A�+A�Q�A�bA��\A�A�5?A���A���A�VA��^A��RA���A�ffA��
A��/A�ȴA��uA���A��mA�VA���A���A�bNA���A�^5A��hA�\)A��A�/A�K�A��FA�1'A�ĜA�t�A�\)A�C�A�E�A��HA��TA���A�K�A���A��hA�1'A�E�A��A��A���A��uA�|�A�S�A�7LA�t�A�+A���A���A��!A�A�A�%A�  A�t�A�Q�A�G�A�7LA�"�A��TA~�Az��Ax��Aup�Aq��AlZAh��AeAcXA_O�A\��A\~�A[AY�PAW`BAV�9AU��AU&�AS��AQ��AO��AM�TAK7LAGhsAE�AD1'AC\)AA��A?|�A<��A<�A;��A:A�A8��A7�PA6ȴA6E�A533A3�FA2n�A1�A01A.bA,�RA+�7A*9XA(�uA&��A%"�A$M�A"�DA ��A E�A|�A~�A �A�PA~�A��AhsA�PA�hAG�A�/A�RA$�A`BA�uA�TA7LA��A$�A  A�A��A�A�A��A��A
z�A�A�PA��A�\A�yA �uA   A M�A �A�A �\@��FA �uAS�A�;A-AƨAdZA�7A\)A?}A �RA 9X@�|�@�j@��m@���@��@�%@�@�33@�hs@�bN@�S�@�j@�"�@�$�@�A�@�;d@�Q�@�S�@�`B@�b@�5?@�M�@�ff@�J@��`@�A�@���@��@Ұ!@�G�@��m@�dZ@��@Ѓ@�~�@���@��@�~�@�M�@�J@��@���@��@Л�@���@���@Гu@���@���@��@�
=@ʇ+@ʧ�@�$�@�r�@�M�@�=q@ǍP@���@��@���@���@ɩ�@�O�@ȣ�@���@�n�@�V@�A�@��T@���@���@�O�@��y@�K�@�ƨ@���@�33@�%@�(�@���@�V@��@���@��@�x�@�ȴ@��
@��R@�l�@��@��@�@���@��@���@�x�@�bN@��m@��@�G�@�=q@�~�@�v�@�~�@�n�@�~�@\@+@�V@��F@�@�%@���@�j@���@���@�V@���@��R@���@�{@�/@��u@��u@��D@�S�@���@�~�@���@�V@�5?@�=q@�{@��#@���@���@�O�@�&�@�?}@��^@�-@�$�@���@�?}@���@��u@�b@��
@���@�33@��y@��\@�V@�J@��@�(�@���@���@��@�b@��@�+@��H@���@�
=@�@���@���@��7@��@���@���@��/@�bN@��u@��@��@�Z@��F@�ȴ@�M�@�@�@�/@���@��u@�A�@��
@�|�@�;d@�@���@���@���@���@�o@���@��F@�t�@�33@�o@��@���@��R@�-@��@�X@��@��D@�Q�@�b@�|�@�@���@�~�@��@��7@��@�X@�?}@���@�ƨ@�5?@��@��u@�j@�Z@�Z@�I�@�A�@� �@���@�o@��@��H@��+@��@�@�hs@��`@��j@��9@�j@�  @���@�l�@�;d@��@�o@�K�@�  @�  @���@��P@��@��F@��@��@��+@��#@�7L@��@��@��9@��D@�9X@��@�C�@�{@�?}@��@~��@x�@k��@a%@Z��@O��@HA�@?�@6��@1�@,(�@'|�@$��@  �@Z@�;@��@K�@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�&�A�&�A�&�A�$�A�&�A�&�A�+A�$�A�&�A�&�A�-A�-A�+A�+A�-A�-A�(�A�(�A�+A�33A�33A�/A�7LA�9XA�;dA�;dA�;dA�=qA�=qA�=qA�?}A�=qA�?}A�7LA�ȴA�|�A��AЏ\A���A��#A�t�A�ȴA�K�A�+A�1'A�^5A�^5AŲ-Aħ�A��
A�+A�Q�A�bA��\A�A�5?A���A���A�VA��^A��RA���A�ffA��
A��/A�ȴA��uA���A��mA�VA���A���A�bNA���A�^5A��hA�\)A��A�/A�K�A��FA�1'A�ĜA�t�A�\)A�C�A�E�A��HA��TA���A�K�A���A��hA�1'A�E�A��A��A���A��uA�|�A�S�A�7LA�t�A�+A���A���A��!A�A�A�%A�  A�t�A�Q�A�G�A�7LA�"�A��TA~�Az��Ax��Aup�Aq��AlZAh��AeAcXA_O�A\��A\~�A[AY�PAW`BAV�9AU��AU&�AS��AQ��AO��AM�TAK7LAGhsAE�AD1'AC\)AA��A?|�A<��A<�A;��A:A�A8��A7�PA6ȴA6E�A533A3�FA2n�A1�A01A.bA,�RA+�7A*9XA(�uA&��A%"�A$M�A"�DA ��A E�A|�A~�A �A�PA~�A��AhsA�PA�hAG�A�/A�RA$�A`BA�uA�TA7LA��A$�A  A�A��A�A�A��A��A
z�A�A�PA��A�\A�yA �uA   A M�A �A�A �\@��FA �uAS�A�;A-AƨAdZA�7A\)A?}A �RA 9X@�|�@�j@��m@���@��@�%@�@�33@�hs@�bN@�S�@�j@�"�@�$�@�A�@�;d@�Q�@�S�@�`B@�b@�5?@�M�@�ff@�J@��`@�A�@���@��@Ұ!@�G�@��m@�dZ@��@Ѓ@�~�@���@��@�~�@�M�@�J@��@���@��@Л�@���@���@Гu@���@���@��@�
=@ʇ+@ʧ�@�$�@�r�@�M�@�=q@ǍP@���@��@���@���@ɩ�@�O�@ȣ�@���@�n�@�V@�A�@��T@���@���@�O�@��y@�K�@�ƨ@���@�33@�%@�(�@���@�V@��@���@��@�x�@�ȴ@��
@��R@�l�@��@��@�@���@��@���@�x�@�bN@��m@��@�G�@�=q@�~�@�v�@�~�@�n�@�~�@\@+@�V@��F@�@�%@���@�j@���@���@�V@���@��R@���@�{@�/@��u@��u@��D@�S�@���@�~�@���@�V@�5?@�=q@�{@��#@���@���@�O�@�&�@�?}@��^@�-@�$�@���@�?}@���@��u@�b@��
@���@�33@��y@��\@�V@�J@��@�(�@���@���@��@�b@��@�+@��H@���@�
=@�@���@���@��7@��@���@���@��/@�bN@��u@��@��@�Z@��F@�ȴ@�M�@�@�@�/@���@��u@�A�@��
@�|�@�;d@�@���@���@���@���@�o@���@��F@�t�@�33@�o@��@���@��R@�-@��@�X@��@��D@�Q�@�b@�|�@�@���@�~�@��@��7@��@�X@�?}@���@�ƨ@�5?@��@��u@�j@�Z@�Z@�I�@�A�@� �@���@�o@��@��H@��+@��@�@�hs@��`@��j@��9@�j@�  @���@�l�@�;d@��@�o@�K�@�  @�  @���@��P@��@��F@��@��@��+@��#@�7L@��@��@��9@��D@�9X@��@�C�G�O�@�?}@��@~��@x�@k��@a%@Z��@O��@HA�@?�@6��@1�@,(�@'|�@$��@  �@Z@�;@��@K�@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BW
BT�BF�B6FBhsB�hB��B�#B��BBuB7LBA�B>wBH�B]/Bp�B�B�DB��B�-BɺB�
B�#B�fB�B�)B�yB��B	7B�B�B��B��B  BBB��B��B��B��B��B�mB�BB�/B�B�
B��B�B�VB��B��B�PBp�B>wB&�B1B�B��B�BoB�B��B��B�'B�B� B^5B[#BgmBYBD�B,B#�BPBB
�B
�B
��B
�B
�1B
s�B
^5B
:^B
�B
B	�B	�B	�FB	�JB	v�B	bNB	O�B	:^B	,B	&�B	�B	�B	JB	B	  B��B�B�mB�5B��BÖB�9B��B��B��B��B��B��B��B��B�oB�VB�PB�bB�bB��B��B��B��B�oB�oB��B��B��B��B��B��B��B��B��B��B��B�B�!B�B��B��B��B�B�FB�}BĜBŢBĜBB�wB�}B��B��BBƨBǮBǮBŢB��B�RB��B�PBz�Br�Bz�B�B�Bw�Bz�B�%B��B��B��B��B�B�XB��BǮBƨB��B�BB�;B�5B�B��B��B��BƨB�qB�B��B��B��B��B��B��B�oB�VB�bB�PB�\B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�3B��B�;B�TB�sB�B�B�B�B��B��B��B	B	B	B	%B	B	B	B	B	+B	%B	B	B	
=B	uB	�B	$�B	-B	0!B	0!B	0!B	0!B	<jB	0!B	+B	'�B	2-B	7LB	6FB	;dB	49B	9XB	>wB	M�B	E�B	?}B	@�B	H�B	R�B	ZB	`BB	]/B	Q�B	G�B	>wB	B�B	M�B	ZB	ffB	iyB	�JB	�{B	��B	��B	��B	��B	��B	�'B	�LB	�XB	�XB	�XB	�XB	�XB	�XB	�RB	�-B	�B	��B	��B	��B	�bB	�VB	�JB	�VB	�VB	�=B	�7B	�+B	�%B	�%B	�+B	�1B	�DB	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�'B	�B	�!B	�B	�B	�!B	�!B	�!B	�-B	�-B	�3B	�3B	�-B	�!B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�RB	�jB	��B	��B	��B	��B	��B	�B	�B	�/B	�/B	�;B	�5B	�/B	�)B	�)B	�/B	�)B	�)B	�#B	�B	�B	�#B	�#B	�#B	�#B	�#B	�)B	�BB	�`B	�mB	�sB	�mB	�sB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�fB	�TB	�NB	�NB	�NB	�TB	�TB	�ZB	�TB	�ZB	�fB	�fB	�fB	�mB	�fB	�`B	�`B	�`B	�fB	�sB	�mB	�mB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
B
1B
�B
�B
&�B
33B
5?B
;dB
B�B
J�B
O�B
W
B
\)B
^5B
cTB
gmB
k�B
n�B
s�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BV�BT�BF�B67BhcB�ZB��B�B��B	BcB7<BA|B>fBH�B]Bp�B��B�2B��B�BɬB��B�B�WB�B�B�lB��B	(B�B�B��B��B��B �B �B��B��B��B��B��B�[B�2B�B�B��B��B�B�EB��B��B�>Bp�B>eB&�B!B�B��B��B[B�B��B�tB�B��B�B^"B[Bg]BYBD�B+�B#�B=B B
�xB
��B
ʳB
��B
�"B
s�B
^)B
:PB
�B
B	�B	�B	�;B	�BB	v�B	bGB	O�B	:XB	,B	&�B	�B	�B	EB	B��B��B�B�lB�4B��B×B�9B��B��B��B��B��B��B��B��B�pB�XB�NB�cB�cB��B��B��B��B�oB�oB��B��B��B��B��B��B��B��B��B��B��B�B� B�B��B��B��B�B�DB�zBęBŞBĘBB�tB�zB��B��BBơBǧBǨBŝB��B�MB��B�QBz�Br�Bz�B�B�Bw�Bz�B�"B��B��B��B��B�B�SB��BǪBơB��B�9B�6B�0B�	B��B��B��BƢB�jB�B��B��B��B�|B��B��B�lB�QB�\B�LB�ZB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�-B��B�5B�MB�mB�wB�vB�xB�B��B��B��B	B	B	B	B	B	B	B	B	 B	B	B	B	
3B	jB	�B	$�B	- B	0B	0B	0B	0B	<`B	0B	*�B	'�B	2#B	7AB	68B	;WB	4,B	9NB	>lB	M�B	E�B	?rB	@xB	H�B	R�B	ZB	`3B	] B	Q�B	G�B	>jB	B�B	M�B	ZB	fXB	ijB	�<B	�kB	��B	��B	��B	��B	��B	�B	�<B	�HB	�IB	�HB	�GB	�HB	�IB	�CB	�B	��B	��B	��B	��B	�UB	�HB	�=B	�IB	�FB	�.B	�)B	�B	�B	�B	�B	�"B	�5B	�LB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�	B	�B	�B	�B	�B	�B	�$B	�#B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�BB	�ZB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�1B	�MB	�[B	�`B	�^B	�dB	�YB	�bB	�lB	�oB	�nB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�nB	�SB	�CB	�9B	�=B	�>B	�BB	�DB	�EB	�CB	�JB	�VB	�VB	�SB	�\B	�TB	�NB	�OB	�PB	�UB	�`B	�YB	�^B	�SB	�aB	�lB	�mB	�oB	�|B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	�B
 �B
!B
|B
�B
&�B
3"B
5,B
;OB
B}B
J�B
O�B
V�B
\B
^B
c@B
gVB
krB
n�B
s�B
u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.19 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436392016080714363920160807143639  AO  ARCAADJP                                                                    20151002192054    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151002192054  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151002192054  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143639  IP                  G�O�G�O�G�O�                