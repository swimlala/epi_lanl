CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-01-19T18:02:52Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �,Argo profile    3.1 1.2 19500101000000  20180119180252  20190405100811  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @�E�ʗ1   @�E⪪� @.!���o�d�-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   A   A   @���@���A   A   A@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B   B  B  B  B   B'��B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�ffB���B�  B�  B�  B�  B�  B�  B�  C   C�C�C  C�fC
  C  C  C  C  C  C  C  C  C�C�fC�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D��D�<�D��3D��fD��D�S3D�p D��3D�3D�33D�|�D�ɚD���D�\�Dڃ3D��D��D�@ D�Y�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��HA
=A'
=AG
=Ag
=A��A��A��A��RAÅAӅA�A�BB	BBB!B)\)B1B9BABIBQBYBaBiBqByB�{B��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HB��HBٮB�G�B�B��HB��HB��HB��HB��HB��HB��HC p�C�>C�>Cp�CW
C
p�Cp�Cp�Cp�Cp�Cp�Cp�Cp�Cp�C�>CW
C W
C"p�C$p�C&p�C(p�C*p�C,p�C.p�C0p�C2p�C4p�C6p�C8p�C:p�C<p�C>p�C@p�CBp�CDp�CFp�CHp�CJp�CLp�CNp�CPp�CRp�CTp�CVp�CXp�CZp�C\p�C^p�C`p�Cbp�Cdp�Cfp�Chp�Cjp�Clp�Cnp�Cpp�Crp�Ctp�Cvp�Cxp�Czp�C|p�C~p�C�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�EC�EC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RC�8RD )D �)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D	)D	�)D
)D
�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D�D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D)D�)D )D �)D!)D!�)D")D"�)D#"�D#�)D$�D$�)D%)D%�)D&)D&�)D')D'�)D()D(�)D))D)�)D*)D*�)D+)D+�)D,)D,�)D-)D-�)D.)D.�)D/)D/�)D0)D0�)D1)D1�)D2)D2�)D3)D3�)D4)D4�)D5)D5�)D6)D6�)D7)D7�)D8)D8�)D9)D9�)D:)D:�)D;)D;�)D<)D<�)D=)D=�)D>)D>�)D?)D?�)D@)D@�)DA)DA�)DB)DB�)DC)DC�)DD)DD�)DE)DE�)DF)DF�)DG)DG�)DH)DH�)DI)DI�)DJ)DJ�)DK)DK�)DL)DL�)DM)DM�)DN)DN�)DO)DO�)DP)DP�)DQ)DQ�)DR)DR�)DS)DS�)DT)DT�)DU)DU�)DV)DV�)DW)DW�)DX)DX�)DY)DY�)DZ)DZ�)D[)D[�)D\)D\�)D])D]�)D^)D^�)D_)D_�)D`)D`�)Da)Da�)Db)Db�)Dc)Dc�)Dd)Dd�)De)De�)Df)Df�)Dg)Dg�)Dh)Dh�)Di)Di�)Dj)Dj�)Dk)Dk�)Dl)Dl�)Dm)Dm�)Dn)Dn�)Do)Do�)Dp)Dp�)Dq)Dq�)Dr)Dr�)Ds)Ds�)Dt)Dt�)Dy��D��D�J�D��GD��zD��D�aGD�~D��GD�GD�AGD���D�׮D��D�j�DڑGD���D��D�ND�g�D��z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA��yA��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A�A�A�%A�%A�%A�%A�
=A�
=A�JA�JA�JA�
=A�
=A�JA�
=A�  A��A��TA��
A���A���A���A�ƨA�A�AмjAк^AиRAв-AЮAЧ�AП�A�jA�-A��A�XA�&�A�r�A�9XA��A�S�A���A�1A��A��A���A���A��-A���A��HA�VA��TA�C�A���A���A�dZA�(�A��DA�r�A�+A�G�A��DA�O�A��HA�^5A�ƨA�;dA��A~M�A}\)AwƨAuS�Atr�Ar�DAnȴAi\)AdZAaC�A^�RA]C�A\�!A\  AZz�AX�AU��AR�!AP��AO?}ANVAL~�AJ�uAH�ABQ�A?G�A>�A>{A=t�A<ffA;A5K�A0ZA-��A+�PA)\)A(�HA(��A(1'A'`BA&^5A$�A#�7A!��A ��A   A�
A��AĜAA�AO�A�!A�uA^5AA�A$�A(�A�AbAbAJA�;AƨA��A�Al�A
=Az�AƨAS�A��AAS�A�AjA�A�hAQ�A��Ar�A�
A��A��A;dA/A�A�hAS�AȴAI�A�A
ĜA
jA
�A	�TA	��AĜAA�
A&�A�yAz�A��A�A��A+A�`A��A�jA��A�\A�DA~�Av�Ar�AQ�A�A&�A�/A�A;dA ��A ��A �+A 5?@�ȴ@���@�hs@�G�@��@�z�@�o@�M�@�%@�A�@�ƨ@���@�S�@��y@�M�@��@��#@���@���@�bN@�1'@�  @��
@���@�=q@�hs@@�n�@�J@���@�r�@�F@�!@�O�@���@�A�@� �@��m@�
=@���@�5?@��T@�-@�`B@�/@�u@�A�@��
@�+@���@��@�O�@�Q�@�b@ߕ�@�S�@�S�@�+@��y@�V@ݺ^@�O�@�%@ܛ�@�1@ۮ@��@�5?@�{@���@���@�X@�1@��@�^5@պ^@�`B@�?}@�7L@�7L@�7L@��@���@Լj@ԃ@�9X@��@�ƨ@Ӯ@�dZ@ҸR@�J@�@�@�@���@�X@У�@ύP@�5?@�V@�Z@�Q�@�9X@�(�@��@�  @�  @��@�ƨ@˝�@�|�@�o@�5?@�/@�z�@��@���@��;@Ǿw@��@�J@őh@�`B@�7L@��@��`@ēu@�z�@�z�@�r�@�r�@�Z@�I�@�9X@� �@�ƨ@�+@��y@�@��T@�O�@�V@���@���@�bN@��;@��@�|�@�
=@���@���@�~�@�ff@�J@���@���@� �@�dZ@�"�@��@��H@���@��\@�V@���@��/@�bN@�  @�dZ@���@�E�@���@�/@�b@�~�@�?}@���@��D@�A�@���@�|�@�"�@��+@���@�hs@��`@���@�Q�@���@��+@�V@�-@��@���@�O�@��/@�j@�bN@�Q�@��@���@�|�@�33@���@�J@���@�V@���@��j@��9@��@��D@�Z@�(�@��@��P@�t�@�dZ@�ȴ@�=q@��@��^@���@���@��h@�p�@���@���@���@���@�l�@�33@�@���@��H@��!@�M�@�{@��@���@�I�@���@��@��@��@�hs@�7L@���@��D@��@�ƨ@�l�@�o@���@�^5@�5?@��@��@�{@�J@��@�@�O�@���@��9@�(�@�|�@�dZ@�;d@�"�@��@��y@�ff@�^5@�^5@�5?@���@��P@��@��@|9X@r�@h�9@a�^@W\)@Q��@I�^@E��@>5?@6$�@-��@&�R@ �u@�@;d@�F@�7@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��yA��yA��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A�A�A�%A�%A�%A�%A�
=A�
=A�JA�JA�JA�
=A�
=A�JA�
=A�  A��A��TA��
A���A���A���A�ƨA�A�AмjAк^AиRAв-AЮAЧ�AП�A�jA�-A��A�XA�&�A�r�A�9XA��A�S�A���A�1A��A��A���A���A��-A���A��HA�VA��TA�C�A���A���A�dZA�(�A��DA�r�A�+A�G�A��DA�O�A��HA�^5A�ƨA�;dA��A~M�A}\)AwƨAuS�Atr�Ar�DAnȴAi\)AdZAaC�A^�RA]C�A\�!A\  AZz�AX�AU��AR�!AP��AO?}ANVAL~�AJ�uAH�ABQ�A?G�A>�A>{A=t�A<ffA;A5K�A0ZA-��A+�PA)\)A(�HA(��A(1'A'`BA&^5A$�A#�7A!��A ��A   A�
A��AĜAA�AO�A�!A�uA^5AA�A$�A(�A�AbAbAJA�;AƨA��A�Al�A
=Az�AƨAS�A��AAS�A�AjA�A�hAQ�A��Ar�A�
A��A��A;dA/A�A�hAS�AȴAI�A�A
ĜA
jA
�A	�TA	��AĜAA�
A&�A�yAz�A��A�A��A+A�`A��A�jA��A�\A�DA~�Av�Ar�AQ�A�A&�A�/A�A;dA ��A ��A �+A 5?@�ȴ@���@�hs@�G�@��@�z�@�o@�M�@�%@�A�@�ƨ@���@�S�@��y@�M�@��@��#@���@���@�bN@�1'@�  @��
@���@�=q@�hs@@�n�@�J@���@�r�@�F@�!@�O�@���@�A�@� �@��m@�
=@���@�5?@��T@�-@�`B@�/@�u@�A�@��
@�+@���@��@�O�@�Q�@�b@ߕ�@�S�@�S�@�+@��y@�V@ݺ^@�O�@�%@ܛ�@�1@ۮ@��@�5?@�{@���@���@�X@�1@��@�^5@պ^@�`B@�?}@�7L@�7L@�7L@��@���@Լj@ԃ@�9X@��@�ƨ@Ӯ@�dZ@ҸR@�J@�@�@�@���@�X@У�@ύP@�5?@�V@�Z@�Q�@�9X@�(�@��@�  @�  @��@�ƨ@˝�@�|�@�o@�5?@�/@�z�@��@���@��;@Ǿw@��@�J@őh@�`B@�7L@��@��`@ēu@�z�@�z�@�r�@�r�@�Z@�I�@�9X@� �@�ƨ@�+@��y@�@��T@�O�@�V@���@���@�bN@��;@��@�|�@�
=@���@���@�~�@�ff@�J@���@���@� �@�dZ@�"�@��@��H@���@��\@�V@���@��/@�bN@�  @�dZ@���@�E�@���@�/@�b@�~�@�?}@���@��D@�A�@���@�|�@�"�@��+@���@�hs@��`@���@�Q�@���@��+@�V@�-@��@���@�O�@��/@�j@�bN@�Q�@��@���@�|�@�33@���@�J@���@�V@���@��j@��9@��@��D@�Z@�(�@��@��P@�t�@�dZ@�ȴ@�=q@��@��^@���@���@��h@�p�@���@���@���@���@�l�@�33@�@���@��H@��!@�M�@�{@��@���@�I�@���@��@��@��@�hs@�7L@���@��D@��@�ƨ@�l�@�o@���@�^5@�5?@��@��@�{@�J@��@�@�O�@���@��9@�(�@�|�@�dZ@�;d@�"�@��@��y@�ff@�^5@�^5@�5?@���@��P@��@��@|9X@r�@h�9@a�^@W\)@Q��@I�^@E��@>5?@6$�@-��@&�R@ �u@�@;d@�F@�7@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBƨBǮBƨBǮBǮBǮBǮBǮBǮBǮBȴBɺBɺB��B��B��B��B��B�5B�B	B	
=B	
=B	DB	DB	DB	DB	DB	
=B	
=B	DB	
=B	DB	DB	JB	�B	+B	;dB	VB	]/B
B
ŢB1'B8RB;dBB�BF�BC�B+B�B!�B{B
�B
ǮB
�7B
u�B
r�B
q�B
o�B
m�B
gmB
VB
A�B
5?B
DB	�sB	ɺB	�XB	�9B	�B	��B	��B	��B	�7B	�B	|�B	u�B	iyB	[#B	O�B	G�B	B�B	?}B	=qB	;dB	6FB	1'B	(�B	�B	�B	�B	�B	oB	JB	B��B��B��B��B��B�B�B�BB�
B��B��B��B��B��B��B��B��BɺB��B��B��B�;B�NB�TB�NB�NB�NB�ZB�fB�B��B��B	B	B	%B	%B	%B	+B	1B	
=B	bB	uB	�B	�B	'�B	/B	6FB	C�B	F�B	K�B	M�B	O�B	VB	aHB	iyB	m�B	t�B	�1B	�JB	�PB	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�'B	�'B	�FB	�XB	�^B	�^B	�^B	�dB	�}B	��B	ÖB	ĜB	ƨB	ɺB	��B	��B	�B	�
B	��B	�B	�B	�)B	�;B	�;B	�;B	�5B	�5B	�5B	�5B	�;B	�;B	�;B	�5B	�/B	�)B	�B	�B	�B	�B	�
B	�B	�
B	�B	�BB	�BB	�BB	�HB	�BB	�BB	�BB	�HB	�HB	�HB	�NB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�`B	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
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
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
DB
DB
JB
JB
JB
JB
JB
JB
JB
JB
JB
PB
PB
JB
PB
VB
VB
VB
VB
VB
VB
VB
VB
VB
\B
\B
bB
bB
bB
hB
hB
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
$�B
%�B
,B
1'B
8RB
<jB
E�B
I�B
O�B
Q�B
VB
YB
]/B
cTB
jB
n�B
t�B
x�B
|�B
�B
�%B
�711111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BƅBƇBƃBƅBƃBƆBƆBƇBƉBƅBƆBƇBƆBƆBƇBǈBƇBǊBǌBǈBǊBǌBǊBǊBȕBɖBɘB̬BϼB��B��B��B�B�mB	 �B	
B	
B	 B	!B	!B	!B	!B	
B	
B	!B	
B	"B	"B	)B	vB	*�B	;@B	U�B	]B
 �B
�}B1B8/B;:BBjBF�BCoB*�B�B!�BQB
�}B
ǇB
�B
u�B
r�B
q�B
otB
mgB
gDB
U�B
A`B
5B
B	�EB	ɍB	�*B	�B	��B	��B	�~B	�SB	�B	��B	|�B	u�B	iJB	Z�B	O�B	G~B	B`B	?MB	=CB	;5B	6B	0�B	(�B	�B	qB	cB	WB	>B	B	�B��B��B��B��B��B�B�SB�B��BѸBΦB̘B˕B˔B˕B˓BʌBɉB˓B̖BҽB�B�B� B�B�B�B�&B�1B�oB��B��B	�B	�B	�B	�B	�B	�B	�B	
B	.B	AB	OB	�B	'�B	.�B	6B	C`B	FuB	K�B	M�B	O�B	U�B	aB	iFB	m_B	t�B	��B	�B	�B	�!B	�TB	�]B	�fB	�vB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�)B	�(B	�)B	�0B	�GB	�UB	�_B	�fB	�rB	ɃB	͜B	ѵB	��B	��B	��B	��B	��B	��B	�B	�B	�B	��B	��B	� B	� B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�	B	�B	�	B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�!B	�)B	�&B	�)B	�+B	�'B	�)B	�0B	�0B	�.B	�.B	�5B	�6B	�;B	�CB	�CB	�BB	�BB	�BB	�MB	�ZB	�_B	�hB	�fB	�gB	�eB	�eB	�eB	�hB	�iB	�kB	�lB	�mB	�lB	�lB	�mB	�mB	�sB	�rB	�xB	�rB	�uB	�pB	�rB	�rB	�xB	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
#B
$B
(B
(B
'B
/B
1B
.B
BB
KB
PB
OB
SB
SB
SB
RB
MB
NB
NB
UB
SB
XB
XB
hB
dB
kB
sB
xB
wB
yB
xB
yB
yB
~B
�B
 �B
�B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
$�B
%�B
+�B
0�B
8B
</B
EiB
I�B
O�B
Q�B
U�B
X�B
\�B
cB
jFB
n_B
t�B
x�B
|�B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.44 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051008112019040510081120190405100811  AO  ARCAADJP                                                                    20180119180252    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20180119180252  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20180119180252  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100811  IP                  G�O�G�O�G�O�                