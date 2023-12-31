CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:13:32Z AOML 3.0 creation; 2016-08-07T21:36:29Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20150226221332  20160807143629  5904461 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5286_8897_013                   2C  D   APEX                            6531                            072314                          846 @�"*��� 1   @�"+@y�@1qhr� ��c�I�^5?1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�  @�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D�	�D�P D���D�ٚD�3D�33D�y�D���D�	�D�6fD�9�D�� D��fD�9�Dډ�D๚D�3D�9�D� D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�
=@�
=A�A#�AC�Ac�A�A�A�A�A�A�A�A�B �HBz�B�HB�HB �HB(�HB0�HB8�HB@�HBH�HBP�HBX�HB`�HBh�HBp�HBx�HB�p�B�p�B�p�B�p�B�p�B���B�
>B�
>B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�=qB�=qB�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B�p�B��B�p�B�p�B�p�C �C8RC8RC8RC8RC
8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC8RC 8RC"8RC$8RC&8RC(8RC*8RC,8RC.8RC08RC28RC48RC68RC88RC:8RC<8RC>8RC@8RCB8RCD8RCF8RCH8RCJ8RCL8RCN8RCP8RCR8RCT8RCV8RCX8RCZ8RC\8RC^8RC`8RCb8RCd8RCf8RCh8RCj8RCl8RCn8RCp8RCr8RCt8RCv8RCx8RCz8RC|8RC~8RC�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�(�C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)C�)D D �DD�DD�DD�DD�DD�DD�DD�DD�D	D	�D
D
�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�DD�D D �D!D!�D"D"�D#D#�D$D$�D%D%�D&D&�D'D'�D(D(�D)D)�D*D*�D+D+�D,D,�D-D-�D.D.�D/D/�D0D0�D1D1�D2D2�D3D3�D4D4�D5D5�D6D6�D7D7�D8D8�D9D9�D:D:�D;D;�D<D<�D=D=�D>D>�D?D?�D@D@�DADA�DBDB�DCDC�DDDD�DEDE�DFDF�DGDG�DHDH�DIDI�DJDJ�DKDK�DLDL�DMDM�DNDN�DODO�DPDP�DQDQ�DRDR�DSDS�DTDT�DUDU�DVDV�DWDW�DXDX�DYDY�DZDZ�D[D[�D\D\�D]D]�D^D^�D_D_�D`D`�DaDa�DbDb�DcDc�DdDd�DeDe�DfDf�DgDg�DhDh�DiDi�DjDj�DkDk�DlDl�DmDm�DnDn�DoDo�DpDp�DqDq�DrDr�DsDs�DtDt�Dt��Dy�D��D�W
D���D��D�
=D�:=D���D���D��D�=pD�@�D��
D��pD�@�Dڐ�D���D�
=D�@�D�
D�Ф111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AمAمAمAه+Aى7Aى7AًDAًDAٍPAٍPAّhAٓuAٓuAٗ�Aٕ�Aٗ�Aٕ�Aٕ�Aٗ�Aٙ�Aٝ�Aٝ�Aٟ�Aٟ�Aٝ�Aٛ�Aٝ�Aٝ�Aٛ�Aٛ�Aٝ�AّhA�z�A�`BA�9XA�AؼjA�K�A��Aω7A̼jA���A�A�=qA��A�bNA�t�Aİ!A�jA��\A�~�A�%A���A�bNA���A��^A�`BA��A�p�A��jA��;A�"�A��A�\)A���A��-A�hsA�A���A�v�A���A�XA���A�/A���A��+A�ffA���A��A���A���A�+A���A�1A���A���A���A�I�A�/A�A�?}A�|�A�^5A�v�A���A��wA�z�A�~�A�ZA�Q�A��/A��A�+A��A~�/A{oAx�`Av��As33Am�Ah1'Ab5?A[�hAW�#AS�wAPv�AN{AK�^AIdZAHr�AEl�AD��AC�AB�\AAhsA@5?A>Q�A=x�A=7LA<�A<��A<��A:�!A8��A733A6�A5�A3��A2bA1\)A0��A.�DA-S�A,ĜA+|�A)�-A'�7A&�A$�/A$5?A#S�A Q�A�A��A?}AA�A�7A/A��A�mA��AO�A�A�A
=A9XA��AS�A�`A�AbNA{A|�A"�A��A�mAC�A��A�DA=qA  A��AdZA�A�uA=qAƨA��AƨAK�A	��A��A��A�A\)A�+AI�A-A�A�PA�;A`BA��A��A�A��A �@���@��P@��y@���@��w@�v�@�/@��y@�$�@�V@��#@��@�!@�$�@���@�1'@�+@�z�@睲@�\)@柾@��@�D@�b@�P@�|�@�l�@�E�@��#@��@߅@�
=@�=q@ݡ�@���@ە�@��H@�ff@�`B@�1@ם�@�+@�^5@�1@ҧ�@��@��@�Ĝ@У�@Л�@Л�@Л�@� �@Χ�@�M�@��@�@�r�@�;d@�V@�-@�@���@ɉ7@��/@���@Ɵ�@��T@��@�Z@�b@�  @�\)@�V@�J@���@�A�@�(�@�(�@�(�@� �@�b@�  @���@��@�ƨ@��@��@�t�@�;d@�o@�5?@�@���@�G�@���@���@�r�@�z�@��D@�b@�o@��@���@���@��!@�M�@��h@�/@��@�bN@�  @��m@���@��;@��;@��m@��@���@�  @��@�b@�1@���@��@���@��@�l�@�l�@�C�@��y@���@���@���@���@�5?@���@�hs@��@�%@��@�Z@���@��
@�ƨ@�S�@��y@�ȴ@���@�~�@��@���@��h@�O�@��/@��@�(�@���@��P@�S�@�
=@��@�ȴ@�-@�hs@��@��@��@�ff@��@�@��-@��@�hs@�/@���@���@��@��R@��+@�=q@���@���@���@�?}@��@��@���@��@�Q�@��m@�l�@�"�@�@���@���@���@���@�M�@���@�O�@�V@��j@���@���@���@���@�Ĝ@��9@�bN@�  @���@�;d@��7@��j@��D@�r�@�9X@��;@��@�C�@�@���@�v�@�n�@�^5@��@��-@�p�@��9@�j@��;@���@�K�@�
=@��!@�E�@�{@���@��@�@���@��@�x�@���@�A�@���@�|�@�S�@�C�@�o@���@���@�=q@��@���@��^@�x�@�7L@�V@��/@���@�z�@�j@�I�@�(�@�(�@�(�@�  @��@��P@�o@�@���@���@�=q@�-@���@��@���@���@���@�9X@���@�ȴ@y��@o|�@eO�@\��@S�F@K33@A��@:=q@1X@+@%��@�y@@ff@�\@��@S�@�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AمAمAمAه+Aى7Aى7AًDAًDAٍPAٍPAّhAٓuAٓuAٗ�Aٕ�Aٗ�Aٕ�Aٕ�Aٗ�Aٙ�Aٝ�Aٝ�Aٟ�Aٟ�Aٝ�Aٛ�Aٝ�Aٝ�Aٛ�Aٛ�Aٝ�AّhA�z�A�`BA�9XA�AؼjA�K�A��Aω7A̼jA���A�A�=qA��A�bNA�t�Aİ!A�jA��\A�~�A�%A���A�bNA���A��^A�`BA��A�p�A��jA��;A�"�A��A�\)A���A��-A�hsA�A���A�v�A���A�XA���A�/A���A��+A�ffA���A��A���A���A�+A���A�1A���A���A���A�I�A�/A�A�?}A�|�A�^5A�v�A���A��wA�z�A�~�A�ZA�Q�A��/A��A�+A��A~�/A{oAx�`Av��As33Am�Ah1'Ab5?A[�hAW�#AS�wAPv�AN{AK�^AIdZAHr�AEl�AD��AC�AB�\AAhsA@5?A>Q�A=x�A=7LA<�A<��A<��A:�!A8��A733A6�A5�A3��A2bA1\)A0��A.�DA-S�A,ĜA+|�A)�-A'�7A&�A$�/A$5?A#S�A Q�A�A��A?}AA�A�7A/A��A�mA��AO�A�A�A
=A9XA��AS�A�`A�AbNA{A|�A"�A��A�mAC�A��A�DA=qA  A��AdZA�A�uA=qAƨA��AƨAK�A	��A��A��A�A\)A�+AI�A-A�A�PA�;A`BA��A��A�A��A �@���@��P@��y@���@��w@�v�@�/@��y@�$�@�V@��#@��@�!@�$�@���@�1'@�+@�z�@睲@�\)@柾@��@�D@�b@�P@�|�@�l�@�E�@��#@��@߅@�
=@�=q@ݡ�@���@ە�@��H@�ff@�`B@�1@ם�@�+@�^5@�1@ҧ�@��@��@�Ĝ@У�@Л�@Л�@Л�@� �@Χ�@�M�@��@�@�r�@�;d@�V@�-@�@���@ɉ7@��/@���@Ɵ�@��T@��@�Z@�b@�  @�\)@�V@�J@���@�A�@�(�@�(�@�(�@� �@�b@�  @���@��@�ƨ@��@��@�t�@�;d@�o@�5?@�@���@�G�@���@���@�r�@�z�@��D@�b@�o@��@���@���@��!@�M�@��h@�/@��@�bN@�  @��m@���@��;@��;@��m@��@���@�  @��@�b@�1@���@��@���@��@�l�@�l�@�C�@��y@���@���@���@���@�5?@���@�hs@��@�%@��@�Z@���@��
@�ƨ@�S�@��y@�ȴ@���@�~�@��@���@��h@�O�@��/@��@�(�@���@��P@�S�@�
=@��@�ȴ@�-@�hs@��@��@��@�ff@��@�@��-@��@�hs@�/@���@���@��@��R@��+@�=q@���@���@���@�?}@��@��@���@��@�Q�@��m@�l�@�"�@�@���@���@���@���@�M�@���@�O�@�V@��j@���@���@���@���@�Ĝ@��9@�bN@�  @���@�;d@��7@��j@��D@�r�@�9X@��;@��@�C�@�@���@�v�@�n�@�^5@��@��-@�p�@��9@�j@��;@���@�K�@�
=@��!@�E�@�{@���@��@�@���@��@�x�@���@�A�@���@�|�@�S�@�C�@�o@���@���@�=q@��@���@��^@�x�@�7L@�V@��/@���@�z�@�j@�I�@�(�@�(�@�(�@�  @��@��P@�o@�@���@���@�=q@�-@���@��@���@���G�O�@�9X@���@�ȴ@y��@o|�@eO�@\��@S�F@K33@A��@:=q@1X@+@%��@�y@@ff@�\@��@S�@�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBbNBbNBcTBbNBbNBbNBbNBbNBbNBcTBbNBbNBbNBbNBbNBbNBbNBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBcTBbNBcTBcTBcTBdZBffBiyBjBhsBjBo�Br�B�B�B�1B�B�7B�7B�\B��B�'BB��B��B�;B�yB��B��BBbB�B�B�B�BbB	7B��BbBuB
=B��B�sB�5B�BɺBĜBǮB�5B�B�B�B#�BZB-B-B33B)�B%BĜB�uB�B{�Bq�BH�BB
�B
��B
�jB
��B
�B
YB
>wB
%�B
{B
DB	��B	�B	��B	�'B	��B	�=B	ffB	D�B	�B��B�B�B��BŢB�wB�XB�?B�-B�!B�B�B�B�B�B�B�B�B�B��B��B�B�B�!B�'B�?B�XB�dB�dB�jB�dB�RB�XB�^B�wB�dB�jB��B��B��B��B��B��B�#B�HB�TB�fB�yB�B�B�B�B�B�B��B��B	  B	B		7B	DB	bB	oB	{B	�B	 �B	"�B	&�B	,B	.B	/B	33B	49B	9XB	>wB	<jB	8RB	2-B	-B	#�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	hB	�B	{B	�B	�B	�B	{B	uB	uB	{B	�B	#�B	�B	uB	\B	\B	\B	
=B	+B	+B	+B	1B	+B	%B	B	B	B	B	B	+B	1B	1B		7B		7B	DB	
=B		7B		7B		7B		7B	
=B	JB	PB	bB	�B	�B	�B	�B	�B	�B	!�B	%�B	&�B	(�B	(�B	)�B	)�B	)�B	(�B	,B	49B	49B	49B	33B	49B	8RB	;dB	<jB	<jB	=qB	=qB	?}B	B�B	E�B	G�B	J�B	L�B	N�B	N�B	Q�B	ZB	ZB	_;B	bNB	e`B	ffB	hsB	jB	k�B	m�B	n�B	o�B	r�B	s�B	u�B	v�B	y�B	|�B	~�B	�B	�B	�B	�%B	�7B	�7B	�=B	�JB	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�-B	�9B	�FB	�dB	�wB	��B	��B	��B	��B	��B	��B	B	ÖB	ŢB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�)B	�/B	�BB	�NB	�ZB	�fB	�TB	�BB	�5B	�)B	�)B	�/B	�5B	�;B	�;B	�;B	�HB	�NB	�HB	�HB	�NB	�TB	�TB	�TB	�ZB	�ZB	�`B	�`B	�fB	�`B	�fB	�mB	�mB	�mB	�sB	�sB	�sB	�sB	�sB	�mB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�mB	�sB	�mB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
bB
{B
�B
$�B
,B
1'B
8RB
>wB
G�B
L�B
S�B
[#B
^5B
e`B
hsB
l�B
p�B
t�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Bb<Bb>BcBBb<Bb>Bb<Bb>Bb>Bb<BcDBb>Bb<Bb>Bb>Bb@Bb<Bb>BcDBcBBcBBcDBcDBcDBcBBcDBcDBcDBcBBb<BcDBcDBcBBdHBfTBikBjqBhaBjnBo�Br�B�B�B� B��B�&B�%B�KB��B�B�B��B��B�(B�gB��B��BBOBnB�B�B�BNB	!B��BMB_B
*B��B�^B�#B�BɧBċBǜB�B�B�B�B#�BZB,�B,�B3B)�BBĉB�cB��B{�Bq�BH�BB
�B
��B
�ZB
��B
��B
YB
>iB
%�B
lB
5B	��B	�B	�~B	�B	��B	�3B	f[B	D�B	�B��B�B�B��BŞB�pB�RB�>B�(B�B�B�B�B�B�B�B�B�
B�B��B��B�B�B�B� B�;B�QB�^B�^B�fB�_B�LB�OB�XB�oB�^B�aB��B��BʼB��B��B��B�B�@B�LB�]B�rB�B�B�B�B�B�B��B��B��B	B		,B	9B	VB	dB	oB	�B	 �B	"�B	&�B	+�B	.	B	/B	3&B	4.B	9IB	>jB	<[B	8GB	2 B	-B	#�B	�B	zB	�B	�B	�B	�B	�B	�B	�B	\B	sB	oB	tB	|B	{B	qB	iB	gB	nB	�B	#�B	�B	iB	OB	QB	PB	
3B	B	B	 B	%B	B	B	B	B	B	B	B	B	&B	&B		*B		*B	8B	
1B		*B		-B		*B		*B	
1B	>B	CB	VB	tB	B	�B	�B	�B	�B	!�B	%�B	&�B	(�B	(�B	)�B	)�B	)�B	(�B	+�B	4+B	4*B	4+B	3#B	4)B	8BB	;UB	<ZB	<[B	=cB	=cB	?mB	B�B	E�B	G�B	J�B	L�B	N�B	N�B	Q�B	ZB	ZB	_+B	b>B	eNB	fWB	hdB	jmB	kvB	m�B	n�B	o�B	r�B	s�B	u�B	v�B	y�B	|�B	~�B	��B	��B	�B	�B	�#B	�&B	�,B	�8B	�>B	�]B	�tB	�sB	�tB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�&B	�5B	�RB	�cB	�qB	�rB	�pB	�rB	�vB	�uB	�zB	�B	ōB	ʭB	˳B	˴B	̹B	;B	ͿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�8B	�CB	�QB	�?B	�-B	�#B	�B	�B	�B	�"B	�$B	�'B	�&B	�4B	�7B	�2B	�2B	�7B	�AB	�>B	�>B	�DB	�CB	�IB	�JB	�SB	�GB	�PB	�YB	�WB	�XB	�_B	�^B	�`B	�_B	�_B	�XB	�XB	�`B	�eB	�kB	�B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�kB	�_B	�XB	�\B	�WB	�XB	�XB	�^B	�`B	�cB	�dB	�iB	�rB	�tB	�vB	�wB	�uB	�tB	�yB	�{B	�|B	�}B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
B
B
B
G�O�B
B
MB
cB
�B
$�B
+�B
1B
8:B
>^B
G�B
L�B
S�B
[B
^B
eHB
hWB
lsB
p�B
t�B
x�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.22 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071436292016080714362920160807143629  AO  ARCAADJP                                                                    20150226221332    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221332  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221332  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807143629  IP                  G�O�G�O�G�O�                