CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:30Z AOML 3.0 creation; 2016-08-07T21:36:29Z UW 3.1 conversion     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150226221330  20160807143629  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_011                   2C  D   APEX                            6531                            072314                          846 @��1   @������@1\�hr��c�E����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDofDo� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D�	�D�P D��3D��3D�	�D�@ D��fD���D��D�I�D�y�D�� D���D��fDڙ�D���D�3D�C3D�ffD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @E@�{@�{A
=A#
=AC
=Ac
=A��A��A��A��A��AхA�A�B BBBB B(B0B8B@BHBPBXB`BhBpBxB�aHB�aHB�aHB�aHB�.B�.B�.B�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB�aHB��{B�{B�aHB�aHB�.B�aHB�aHB�aHC 0�C0�C0�C0�C0�C
0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C0�C 0�C"0�C$0�C&0�C(0�C*0�C,0�C.0�C00�C20�C40�C60�C80�C:0�C<0�C>0�C@0�CB0�CD0�CF0�CH0�CJ0�CL
CN0�CP0�CR0�CT0�CV0�CX0�CZ0�C\0�C^0�C`0�Cb0�Cd0�Cf0�Ch0�Cj0�Cl0�Cn0�Cp0�Cr0�Ct0�Cv0�Cx0�Cz0�C|0�C~0�C�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#)D#�)D$)D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC��DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn��Do�Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)Dt�\Dy��D��D�VD��GD��GD��D�FD��zD���D��D�O�D��D��D��D��zDڟ�D���D�	GD�IGD�lzD���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AڮAک�Aڥ�Aڟ�AځA�VA�  A���A�ƨA���Aٴ9AٮA٧�A٥�A٣�A١�A١�Aٟ�Aٝ�Aٛ�Aٙ�Aٙ�Aٙ�Aٙ�Aٗ�Aٗ�Aٗ�Aٕ�AٓuAّhAٍPAًDAمA�~�A�x�A�K�A׾wA���A���A�r�A�=qA�+A�VA���A�ȴAũ�Ać+A��A¾wA�VA�ZA��!A�oA�S�A�\)A���A�  A��mA��jA���A��wA��wA�(�A�z�A�ZA��RA���A�A�33A�{A��hA�|�A�`BA���A�%A�hsA��uA�/A�1'A��7A���A�O�A�x�A�r�A��!A���A�  A��A���A�ZA��HA��DA��A�^5A}�-Ax�AtQ�Ar(�Ao�Ak�Ag�PAb�!A^�A\��AZ��AYG�ASp�AP�AN�jAM�AM`BAL��AK\)AF�/AD�+ABA@�A@{A>I�A<�A:1A8�A8$�A6�yA4��A3�A3C�A2v�A1O�A/A,�+A*��A(ĜA(-A'oA%�A#VA"ZA!�A M�A�Ax�A"�A�A&�AI�A�FAoAQ�Al�A�A(�AA��Al�AVA�RAbNA�#A��A��A��A�uA�AjA9XA{AhsA�jAM�AA�AdZA�PA��A��A��Al�A�AJA%AbNAdZA�A
�HA
=qA	�TA	hsA	K�AĜAJA��A�7A�PA�A^5A��AO�A"�A
=A�A��A�-A��Ax�Al�AS�A33A�yA�RA-A-A�#Al�A7LA �A ��A bN@���@�C�@��y@�5?@�Z@��7@��;@�C�@���@��j@�  @�+@��@�!@�n�@���@�O�@� �@�v�@��T@�;d@�z�@�@@��y@��@�1@�7L@�P@�!@䛦@◍@�=q@��/@�M�@ۍP@�
=@ڰ!@�^5@�$�@ڏ\@�M�@���@�?}@� �@��;@��;@�\)@�+@��@�M�@��@�^5@д9@�Q�@�v�@�$�@��@͡�@�Ĝ@�9X@��@���@���@ʟ�@���@�X@���@�A�@��@ũ�@�x�@�/@Ĭ@�I�@�  @�l�@§�@�@�@���@��h@��@��@���@���@�Ĝ@�Q�@�(�@�Z@�1'@�1@�33@��H@���@�=q@�hs@��@�z�@� �@�;d@�V@��@�X@��/@��9@��9@��D@�1'@��m@�S�@���@�ȴ@��!@��+@�M�@��@�`B@�V@��D@�j@�1'@��@���@�l�@�C�@�C�@�33@��y@��\@�=q@�J@��^@��@��9@��@���@�S�@��H@�n�@�=q@��@���@��^@���@�V@�Ĝ@�A�@�  @��
@���@�l�@�S�@�ȴ@���@��\@�5?@��@���@���@�r�@�(�@�b@�  @��@�"�@���@�p�@��u@��@�  @��D@��u@�A�@�ƨ@��P@�ff@�hs@�%@��@��u@��@�A�@���@��P@�dZ@�;d@�o@��y@���@��!@���@���@�v�@�=q@��@��-@�O�@�?}@�%@���@���@��@�Z@�I�@��@��
@���@�|�@�S�@��@��@���@��!@�v�@�E�@�5?@�$�@��@���@�p�@�O�@�V@���@��`@���@��/@�Z@��
@��P@�\)@�;d@��H@���@��+@�v�@�-@�@�p�@�7L@��@���@�r�@�Q�@� �@��F@�|�@�C�@��H@�ȴ@��R@�v�@�E�@�@��h@�7L@�V@���@��@�j@�bN@�A�@�(�@�ƨ@���@�t�@�S�@�+@��@���@�E�@��@���@�7L@��y@|9X@rn�@h�u@bn�@X �@O
=@F��@<1@3"�@.�y@(  @$�j@�T@��@`B@��@O�@C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 AڮAک�Aڥ�Aڟ�AځA�VA�  A���A�ƨA���Aٴ9AٮA٧�A٥�A٣�A١�A١�Aٟ�Aٝ�Aٛ�Aٙ�Aٙ�Aٙ�Aٙ�Aٗ�Aٗ�Aٗ�Aٕ�AٓuAّhAٍPAًDAمA�~�A�x�A�K�A׾wA���A���A�r�A�=qA�+A�VA���A�ȴAũ�Ać+A��A¾wA�VA�ZA��!A�oA�S�A�\)A���A�  A��mA��jA���A��wA��wA�(�A�z�A�ZA��RA���A�A�33A�{A��hA�|�A�`BA���A�%A�hsA��uA�/A�1'A��7A���A�O�A�x�A�r�A��!A���A�  A��A���A�ZA��HA��DA��A�^5A}�-Ax�AtQ�Ar(�Ao�Ak�Ag�PAb�!A^�A\��AZ��AYG�ASp�AP�AN�jAM�AM`BAL��AK\)AF�/AD�+ABA@�A@{A>I�A<�A:1A8�A8$�A6�yA4��A3�A3C�A2v�A1O�A/A,�+A*��A(ĜA(-A'oA%�A#VA"ZA!�A M�A�Ax�A"�A�A&�AI�A�FAoAQ�Al�A�A(�AA��Al�AVA�RAbNA�#A��A��A��A�uA�AjA9XA{AhsA�jAM�AA�AdZA�PA��A��A��Al�A�AJA%AbNAdZA�A
�HA
=qA	�TA	hsA	K�AĜAJA��A�7A�PA�A^5A��AO�A"�A
=A�A��A�-A��Ax�Al�AS�A33A�yA�RA-A-A�#Al�A7LA �A ��A bN@���@�C�@��y@�5?@�Z@��7@��;@�C�@���@��j@�  @�+@��@�!@�n�@���@�O�@� �@�v�@��T@�;d@�z�@�@@��y@��@�1@�7L@�P@�!@䛦@◍@�=q@��/@�M�@ۍP@�
=@ڰ!@�^5@�$�@ڏ\@�M�@���@�?}@� �@��;@��;@�\)@�+@��@�M�@��@�^5@д9@�Q�@�v�@�$�@��@͡�@�Ĝ@�9X@��@���@���@ʟ�@���@�X@���@�A�@��@ũ�@�x�@�/@Ĭ@�I�@�  @�l�@§�@�@�@���@��h@��@��@���@���@�Ĝ@�Q�@�(�@�Z@�1'@�1@�33@��H@���@�=q@�hs@��@�z�@� �@�;d@�V@��@�X@��/@��9@��9@��D@�1'@��m@�S�@���@�ȴ@��!@��+@�M�@��@�`B@�V@��D@�j@�1'@��@���@�l�@�C�@�C�@�33@��y@��\@�=q@�J@��^@��@��9@��@���@�S�@��H@�n�@�=q@��@���@��^@���@�V@�Ĝ@�A�@�  @��
@���@�l�@�S�@�ȴ@���@��\@�5?@��@���@���@�r�@�(�@�b@�  @��@�"�@���@�p�@��u@��@�  @��D@��u@�A�@�ƨ@��P@�ff@�hs@�%@��@��u@��@�A�@���@��P@�dZ@�;d@�o@��y@���@��!@���@���@�v�@�=q@��@��-@�O�@�?}@�%@���@���@��@�Z@�I�@��@��
@���@�|�@�S�@��@��@���@��!@�v�@�E�@�5?@�$�@��@���@�p�@�O�@�V@���@��`@���@��/@�Z@��
@��P@�\)@�;d@��H@���@��+@�v�@�-@�@�p�@�7L@��@���@�r�@�Q�@� �@��F@�|�@�C�@��H@�ȴ@��R@�v�@�E�@�@��h@�7L@�V@���@��@�j@�bN@�A�@�(�@�ƨ@���@�t�@�S�@�+@��@���@�E�G�O�@���@�7L@��y@|9X@rn�@h�u@bn�@X �@O
=@F��@<1@3"�@.�y@(  @$�j@�T@��@`B@��@O�@C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBk�Bk�Bk�BjBiyBhsBffBe`BdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBdZBe`Bq�B��B�'B�9B�^B�9B�!B�}B�}B�HB  B  BJB
=B{B+B8RBC�B�+B�?BǮB�B��B��BYB&�B�B  B��B�BB�mB�ZB��B�qB�jB�B�hB�%B� Bt�Bo�BdZBO�B0!BB
�B
�TB
��B
�3B
�B
aHB
L�B
;dB
&�B
bB	��B	�;B	ǮB	��B	�VB	|�B	k�B	VB	?}B	%�B	uB	1B��B�B�5B��B��BǮBŢBÖB�qB�9B�'B�B�B��B��B��B��B�B�B�9B�LB�RB�RB�wB�}BÖBÖBÖBƨB��B��B��B�B�#B�/B�/B�NB�ZB�ZB�B�B��B��B��B	B	B	1B	{B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	'�B	)�B	+B	+B	)�B	&�B	#�B	#�B	�B	�B	�B	,B	33B	49B	5?B	5?B	?}B	B�B	?}B	;dB	8RB	:^B	=qB	<jB	;dB	6FB	49B	2-B	7LB	6FB	7LB	;dB	<jB	<jB	<jB	;dB	<jB	<jB	<jB	=qB	?}B	@�B	A�B	A�B	A�B	@�B	@�B	B�B	F�B	E�B	F�B	G�B	H�B	G�B	G�B	F�B	H�B	J�B	J�B	J�B	J�B	D�B	B�B	@�B	;dB	7LB	49B	1'B	1'B	1'B	;dB	?}B	=qB	8RB	33B	5?B	A�B	Q�B	R�B	O�B	R�B	VB	M�B	G�B	D�B	D�B	@�B	;dB	:^B	6FB	1'B	-B	,B	+B	+B	+B	33B	7LB	9XB	9XB	;dB	<jB	?}B	@�B	@�B	?}B	=qB	6FB	33B	:^B	C�B	D�B	D�B	D�B	B�B	A�B	B�B	I�B	L�B	F�B	G�B	F�B	E�B	F�B	P�B	Q�B	ZB	]/B	_;B	aHB	aHB	aHB	aHB	bNB	e`B	gmB	hsB	hsB	jB	p�B	s�B	s�B	w�B	z�B	z�B	}�B	� B	� B	�B	�B	�1B	�=B	�DB	�VB	�bB	�bB	�\B	�\B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�3B	�?B	�FB	�LB	�RB	�RB	�RB	�jB	�jB	��B	B	B	B	B	B	B	B	ÖB	ĜB	ĜB	ƨB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�)B	�)B	�)B	�#B	�B	�B	�#B	�/B	�NB	�ZB	�ZB	�ZB	�NB	�;B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�HB	�HB	�HB	�NB	�TB	�TB	�TB	�TB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
	7B
\B
oB
�B
#�B
(�B
.B
49B
<jB
A�B
K�B
Q�B
VB
]/B
`BB
ffB
iyB
m�B
r�B
v�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 BkwBkuBkwBjrBijBheBfYBeTBdIBdLBdLBdGBdLBdIBdIBdLBdLBdLBdMBdMBdMBdMBdJBdMBdJBdOBdMBdOBdMBdMBdJBdMBdJBdMBdMBeSBq�B��B�B�)B�MB�)B�B�pB�mB�6B��B��B7B
.BlB*�B8BBC�B�B�/BǟB�B��B��BYB&�B�B��B��BrB �B�\B�IB̽B�_B�YB�B�VB�B�Bt�Bo�BdIBO�B0B�B
�B
�EB
��B
�%B
��B
a7B
L�B
;WB
&�B
VB	��B	�/B	ǧB	��B	�MB	|�B	kB	U�B	?wB	%�B	rB	.B��B�B�4B��B��BǫBŞBÖB�oB�8B�%B�B�B��B��B��B��B�B� B�9B�JB�NB�PB�uB�zBÓBÔBÓBƣB��B��B��B�	B�B�+B�*B�KB�WB�UB�B�B��B��B��B	B	B	*B	tB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	'�B	)�B	*�B	*�B	)�B	&�B	#�B	#�B	�B	yB	�B	+�B	3(B	42B	56B	57B	?vB	B�B	?tB	;]B	8IB	:VB	=hB	<_B	;[B	6;B	4,B	2#B	7CB	6=B	7CB	;ZB	<_B	<`B	<`B	;[B	<`B	<`B	<aB	=gB	?sB	@|B	A�B	AB	A~B	@{B	@yB	B�B	F�B	E�B	F�B	G�B	H�B	G�B	G�B	F�B	H�B	J�B	J�B	J�B	J�B	D�B	B�B	@yB	;\B	7BB	4/B	1B	1B	1B	;YB	?uB	=hB	8FB	3+B	55B	AB	Q�B	R�B	O�B	R�B	U�B	M�B	G�B	D�B	D�B	@yB	;ZB	:UB	6<B	1B	-B	+�B	*�B	*�B	*�B	3)B	7@B	9LB	9LB	;ZB	<_B	?qB	@yB	@xB	?rB	=fB	6<B	3(B	:SB	C�B	D�B	D�B	D�B	B�B	A~B	B�B	I�B	L�B	F�B	G�B	F�B	E�B	F�B	P�B	Q�B	ZB	]#B	_1B	a;B	a=B	a<B	a;B	bBB	eRB	g_B	hgB	hfB	jrB	p�B	s�B	s�B	w�B	z�B	z�B	}�B	�B	�B	��B	�B	�"B	�-B	�5B	�HB	�UB	�TB	�NB	�OB	�B	�zB	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�$B	�1B	�6B	�;B	�AB	�BB	�EB	�\B	�\B	�uB	�B	�~B	B	B	�B	B	�B	ÇB	ČB	ČB	ƗB	ɪB	ɫB	ɪB	ɪB	˸B	̼B	��B	��B	��B	��B	��B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�;B	�HB	�JB	�IB	�;B	�)B	�B	�B	�$B	�$B	�&B	�%B	�)B	�/B	�1B	�5B	�9B	�9B	�7B	�;B	�BB	�CB	�BB	�DB	�HB	�KB	�HB	�PB	�QB	�PB	�TB	�UB	�\B	�^B	�_B	�gB	�gB	�gB	�gB	�iB	�lB	�mB	�lB	�tB	�uB	�xB	�xB	�|B	�zB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
G�O�B
	%B
HB
\B
�B
#�B
(�B
.B
4$B
<WB
AtB
K�B
Q�B
U�B
]B
`-B
fSB
ieB
m}B
r�B
v�B
x�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.19 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436292016080714362920160807143629  AO  ARCAADJP                                                                    20150226221330    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221330  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221330  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143629  IP                  G�O�G�O�G�O�                