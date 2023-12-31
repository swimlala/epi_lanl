CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-01-24T22:34:41Z creation; 2021-02-04T21:07:28Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  cH   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ƹ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 1�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 9x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � _�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ~P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ~�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20210124223441  20210204202731  5905790 5905790 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               f   fAA  AOAO7824_008764_102                 7824_008764_102                 2C  2C  DD  SOLO_II                         SOLO_II                         8764                            8764                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @�Y6w�d@�Y6w�d11  @�Y6��}V@�Y6��}V@6j��
@6j��
�d��^ F��d��^ F�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@=p�@�  @�G�@��R@�p�A   A��A   A,(�A?\)A`  A�  A�  A�  A��A��A�Q�A�  A�B   B  B�
B  B (�B'�
B/�
B7�B?�BH(�BP(�BW�
B`  Bh(�Bp(�Bx  B�B��B��
B�  B�  B��B�  B�  B�{B��B��B��B�{B�{B�{B�{B�  B�{B�  B�  B�  B�  B�  B�  B�  B�  B�  B�{B�  B��B��B�{C {C
=C
=C
=C��C	��C  C
=C
=C  C  C  C  C
=C
=C��C��C"  C#��C&  C(
=C*  C,
=C.
=C0  C1��C3��C5��C7��C9��C<  C>  C@  CB  CD  CF  CH
=CJ  CK��CN  CP  CR
=CT  CV  CX  CY��C\  C^
=C`  Cb  Cd  Cf  Ch  Ci��Cl  Cn
=Cp
=Cr  Cs�Cv  Cx
=Cz  C|  C~
=C�  C�  C���C���C�  C�  C���C���C�C�  C�C�C�  C�C�C�C���C���C�C�C�C�  C�  C�C�  C���C���C�  C�C�  C�  C���C�  C�  C���C���C���C�  C���C�  C�C�  C�  C�  C���C�  C�C���C���C���C�  C�  C�  C���C�  C�  C�  C�  C�  C�C�C�  C�  C�  C�C�C�  C���C�  C�C�  C���C�  C���C���C�  C�C�C�  C�  C�  C�  C�  C�C�C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�  C���C�  C�C�  C�  C�  C�C�C�C�
=C�C���C���C�C�C�  C���C���C���C�  C���C�  C�C�C�C�  C���C���C�  C���C���C���D }qD  D��D�D� D  D� D�qD� D  D� D  D� D  D� D  D}qD�qD	� D
  D
}qD
�qD� D  D}qD  D� D�qD��D�D� D�qD}qD  D� D�D��D�D� D�qD� D  D� D�qD}qD  D��D  D� D�D� D�D� D  D}qD�qD� D�D��D  D}qD  D� D�qD }qD �qD!}qD!�qD"}qD"�qD#}qD$  D$��D%�D%��D&  D&� D'  D'}qD(  D(��D)�D)��D*  D*��D+�D+� D,�D,��D,�qD-}qD-�qD.� D/�D/� D/�qD0� D1  D1� D2  D2��D3  D3� D3�qD4}qD5�D5��D5�qD6}qD7  D7��D7�qD8z�D8�qD9� D9�qD:� D;�D;}qD<  D<� D<�qD=� D>�D>� D>�qD?� D@D@� DA  DA��DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI�DI��DJ  DJ� DJ�qDK� DL  DL� DL�qDM}qDN  DN� DO  DO� DP  DP� DP�qDQ}qDR  DR��DS  DS��DT�DT��DU  DU� DV  DV� DW  DWz�DW�qDX}qDX�qDY� DZ  DZ��D[  D[� D\  D\� D]  D]� D^�D^� D^�qD_� D_�qD`}qDa�Da� Db  Db��Dc  Dc� Dd  Dd}qDd�qDe}qDf  Df� Dg�Dg}qDg�qDh� DiDi��Dj�Dj� Dk  Dk��Dl�Dl� Dl�qDm}qDn�Dn}qDo  Do��Dp�Dp��Dq  Dq}qDr  Dr��Ds�Ds� Ds�qDt� Du�Du� Du�qDv}qDw  Dw��Dx�Dx� Dy�Dy�Dz�Dz� D{  D{� D|�D|��D}  D}� D~  D~� D~�qD� D�  D�AHD��HD�� D�  D�AHD��HD�� D�  D�@ D�� D�� D���D�>�D�~�D�� D�HD�>�D�}qD�� D�  D�@ D�� D���D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�@ D�� D��HD�  D�@ D�~�D���D�  D�>�D�~�D��HD�HD�>�D�� D�� D���D�@ D��HD�� D�HD�AHD�� D��qD���D�>�D�� D�� D�  D�>�D��HD�� D�  D�@ D�}qD���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD��HD���D�>�D��HD��HD�  D�>�D�� D�� D�  D�AHD�� D��HD��D�@ D�~�D���D�  D�AHD�� D���D���D�@ D��HD�� D�  D�>�D�� D��HD�  D�AHD��HD�� D���D�@ D��HD�� D�  D�@ D��HD��HD�  D�@ D�~�D���D�  D�AHD��HD�� D�  D�>�D�~�D���D���D�@ D�~�D���D�  D�@ D�~�D���D�HD�AHD�� D���D���D�@ D�� D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D���D�@ D��HD�D�  D�@ D��HD�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D�HD�AHD�� D�� D���D�>�D�~�D�� D�HD�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�� D�� D�HD�AHD��HD���D�  D�AHD�� D�� D�  D�>�D�� D�� D�  D�>�D�� D�� D�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�@ DHD�� D�  D�@ DÀ D��HD�HD�AHDĀ D�� D�  D�AHDŁHD��HD�  D�@ Dƀ Dƾ�D���D�@ Dǀ D�� D���D�@ DȁHD��HD�  D�@ DɁHD�� D�  D�@ D�~�DʽqD���D�@ DˁHD�� D�  D�AHD̀ D��HD�  D�@ D�~�D;�D�  D�@ D΁HD�� D�HD�AHDρHD�� D���D�>�D�}qDнqD�  D�@ D�~�DѾ�D���D�@ DҀ D�� D�  D�AHDӁHD�� D�  D�@ DԀ D��HD�HD�@ DՀ D��HD�  D�@ DցHD�� D���D�@ D׀ D׾�D���D�@ D؀ D��HD�  D�@ D�~�DٽqD���D�@ D�~�DڽqD�  D�@ D�~�D۾�D�HD�AHD܁HD�� D���D�>�D݀ D�� D�  D�@ DށHD�� D�  D�AHD߁HD��HD�HD�AHD��HD�� D�  D�@ D� D�� D�  D�>�D�~�D�� D�  D�AHD�HD�� D�  D�@ D� D��HD�HD�>�D�~�D�� D�  D�AHD� D澸D�  D�AHD� D羸D�  D�@ D� D�� D�  D�AHD� D�� D�  D�@ D� D�� D�HD�@ D� D��HD�HD�@ D�~�D�� D�  D�AHD� D���D�  D�@ D�HD��HD�HD�@ D� D�� D�  D�@ D�� D�D�HD�AHD� D�� D���D�@ D�HD��HD�  D�>�>�?.{?aG�?��R?Ǯ?�ff@�@
=@+�@:�H@Q�@aG�@z�H@��
@���@�
=@�G�@�=q@�33@�(�@�ff@У�@�Q�@�G�@�{@�
=A   A�A	��A\)Az�AQ�A�RA$z�A)��A.{A3�
A9��A>{AC33AH��AMp�AR�\AXQ�A\��Ab�\AhQ�Al��Aq�Aw�A|(�A���A�33A�A���A��A�{A�Q�A��A�A���A�33A�{A���A��A�{A���A�33A�{A�Q�A��HA��A�  A��\A��A��A\A���AǮA�=qA���AϮA�=qA���A�\)A��A���A߮A�=qA�z�A�
=A��A�z�A�
=A��A�z�A�\)A��A�z�A�\)B�B�\B�BG�B�RB�
B	G�B
�RB(�Bp�B�HB(�B��B�RBQ�B��B
=Bz�B�B
=B��B�B�B ��B"=qB#�B$��B&=qB'�
B)G�B*�\B,  B-G�B.�RB0(�B1p�B2�HB4Q�B5B7
=B8Q�B9B;\)B<��B=�B?\)B@��BB=qBC�BD��BFffBG�
BI�BJffBL  BMG�BN�HBP(�BQp�BR�HBTQ�BU�BW33BXz�BY�B[�B\��B^{B_�Ba�BbffBd  BeG�Bf�RBh(�Bi��Bj�HBlQ�Bm��Bo33Bpz�BqBs\)Bt��Bu�Bw\)Bx��By�B{�B|��B}�B�B�z�B�
=B��B�ffB�
=B�B�ffB��B��B�ffB�
=B��B�ffB��B��B�ffB�
=B�B�Q�B�
=B��B�Q�B���B��B�=qB�
=B���B�=qB���B��B�=qB��HB��B�=qB���B�p�B�(�B���B�p�B�  B��RB�\)B�  B���B�G�B��B��\B�33B��
B��\B�33B��
B�z�B��B��
B�ffB�
=B�B�Q�B���B���B�=qB���B��B�{B��RB�\)B��B��\B�33B��B�=qB���B�p�B��B�ffB���B�\)B�B�(�B��\B��HB�\)B�B�{B��\B���B�\)B�B�=qB���B���B�p�B��
B�(�B���B�
=B�\)B��
B�=qB���B�
=B�p�B��
B�Q�B���B�
=B�p�B�B�Q�B���B���B�\)B��
B�=qB��\B�
=B�\)B�B�(�B£�B���B�G�B�B�(�Bď\B��HB�G�BŮB�{BƏ\B��HB�G�BǙ�B�{B�ffB���B�33Bə�B�  B�ffB���B��B˙�B��B�Q�B̸RB��B�p�B��B�=qBΣ�B�
=B�p�B��B�(�BЏ\B�
=B�\)B�B�{B�z�B��HB�G�Bә�B�{B�ffB���B�33BՅB��B�=qB֣�B�
=B�\)B�B�{B�z�B���B�33Bٙ�B�  B�Q�Bڣ�B�
=B�\)B�B�(�B�z�B���B�33Bݙ�B��B�=qBޣ�B���B�\)B߮B�(�B��B���B�\)B��
B�=qB�RB�
=B�B�  B�z�B��HB�\)B��B�=qB�RB�G�B�B�=qB�\B�
=B�B�  B�z�B���B�p�B��B�ffB���B�\)B��B�Q�B���B�\)B��B�ffB��HB�\)B��
B�ffB��HB�p�B��B�z�B���B�p�B�  B�z�B�
=B��B�  B��\B�
=B���B�(�B��RB�G�B��
B�Q�B��HB��B�  B��\B��B�C (�C p�C �C ��CG�C�\C�
C�CffC�C�C33Cz�CC
=CQ�C��C�
C�CffC�C��C=qC�C��C{C\)C�C��C=qC�\C��C	(�C	p�C	C

=C
Q�C
��C
�C33C�CC{CQ�C��C�C=qC�\C�
C�Cp�C�C  CG�C��C�HC(�Cz�CC{C\)C��C�C33C�C��C{C\)C�C��C=qC�\C��C(�Cp�C�RC
=CQ�C��C�HC33Cp�C��C{C\)C�C�C=qC�C��C�C\)C�C��CG�C��C�HC(�Cz�CC
=C\)C��C��C=qC�\C�HC(�Cp�C�RC 
=C \)C �C ��C!G�C!�\C!�HC"(�C"p�C"C#{C#\)C#��C#��C$=qC$�\C$�
C%(�C%p�C%�RC&  C&Q�C&�\C&�
C'{C'\)C'��C'�HC(�C(\)C(�C(C)
=C)33C)p�C)��C)�
C*
=C*G�C*z�C*�C*�HC+�C+Q�C+�\C+C,  C,33C,\)C,��C,�
C-{C-G�C-z�C-�RC-�C.�C.Q�C.�\C.��C/  C/33C/ffC/��C/�
C0
=C0=qC0z�C0�C0�HC1�C1Q�C1�C1C1�C2(�C2\)C2�\C2�
C3
=C3G�C3z�C3�C3�HC4�C4\)C4�\C4��C5  C5=qC5p�C5�C5�C6�C6\)C6��C6��C7
=C7=qC7z�C7�C7�C8�C8\)C8��C8��C9
=C9=qC9�C9�RC9��C:33C:p�C:�C:�HC;(�C;\)C;�\C;��C<
=C<G�C<�C<�RC<��C=33C=ffC=��C=�HC>�C>\)C>�\C>��C?  C?=qC?p�C?�RC?��C@(�C@\)C@��C@�HCA{CAQ�CA�\CACB  CB=qCB�CB�RCB�CC(�CCffCC��CC�HCD{CDQ�CD��CD�
CE
=CEG�CEz�CECE��CF33CFz�CF�RCF��CG(�CGffCG�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                     ?�  ?��H@=p�@�  @�G�@��R@�p�A   A��A   A,(�A?\)A`  A�  A�  A�  A��A��A�Q�A�  A�B   B  B�
B  B (�B'�
B/�
B7�B?�BH(�BP(�BW�
B`  Bh(�Bp(�Bx  B�B��B��
B�  B�  B��B�  B�  B�{B��B��B��B�{B�{B�{B�{B�  B�{B�  B�  B�  B�  B�  B�  B�  B�  B�  B�{B�  B��B��B�{C {C
=C
=C
=C��C	��C  C
=C
=C  C  C  C  C
=C
=C��C��C"  C#��C&  C(
=C*  C,
=C.
=C0  C1��C3��C5��C7��C9��C<  C>  C@  CB  CD  CF  CH
=CJ  CK��CN  CP  CR
=CT  CV  CX  CY��C\  C^
=C`  Cb  Cd  Cf  Ch  Ci��Cl  Cn
=Cp
=Cr  Cs�Cv  Cx
=Cz  C|  C~
=C�  C�  C���C���C�  C�  C���C���C�C�  C�C�C�  C�C�C�C���C���C�C�C�C�  C�  C�C�  C���C���C�  C�C�  C�  C���C�  C�  C���C���C���C�  C���C�  C�C�  C�  C�  C���C�  C�C���C���C���C�  C�  C�  C���C�  C�  C�  C�  C�  C�C�C�  C�  C�  C�C�C�  C���C�  C�C�  C���C�  C���C���C�  C�C�C�  C�  C�  C�  C�  C�C�C�  C�  C�  C�  C�  C�  C�  C���C���C�  C�  C�  C���C�  C�C�  C�  C�  C�C�C�C�
=C�C���C���C�C�C�  C���C���C���C�  C���C�  C�C�C�C�  C���C���C�  C���C���C���D }qD  D��D�D� D  D� D�qD� D  D� D  D� D  D� D  D}qD�qD	� D
  D
}qD
�qD� D  D}qD  D� D�qD��D�D� D�qD}qD  D� D�D��D�D� D�qD� D  D� D�qD}qD  D��D  D� D�D� D�D� D  D}qD�qD� D�D��D  D}qD  D� D�qD }qD �qD!}qD!�qD"}qD"�qD#}qD$  D$��D%�D%��D&  D&� D'  D'}qD(  D(��D)�D)��D*  D*��D+�D+� D,�D,��D,�qD-}qD-�qD.� D/�D/� D/�qD0� D1  D1� D2  D2��D3  D3� D3�qD4}qD5�D5��D5�qD6}qD7  D7��D7�qD8z�D8�qD9� D9�qD:� D;�D;}qD<  D<� D<�qD=� D>�D>� D>�qD?� D@D@� DA  DA��DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI�DI��DJ  DJ� DJ�qDK� DL  DL� DL�qDM}qDN  DN� DO  DO� DP  DP� DP�qDQ}qDR  DR��DS  DS��DT�DT��DU  DU� DV  DV� DW  DWz�DW�qDX}qDX�qDY� DZ  DZ��D[  D[� D\  D\� D]  D]� D^�D^� D^�qD_� D_�qD`}qDa�Da� Db  Db��Dc  Dc� Dd  Dd}qDd�qDe}qDf  Df� Dg�Dg}qDg�qDh� DiDi��Dj�Dj� Dk  Dk��Dl�Dl� Dl�qDm}qDn�Dn}qDo  Do��Dp�Dp��Dq  Dq}qDr  Dr��Ds�Ds� Ds�qDt� Du�Du� Du�qDv}qDw  Dw��Dx�Dx� Dy�Dy�Dz�Dz� D{  D{� D|�D|��D}  D}� D~  D~� D~�qD� D�  D�AHD��HD�� D�  D�AHD��HD�� D�  D�@ D�� D�� D���D�>�D�~�D�� D�HD�>�D�}qD�� D�  D�@ D�� D���D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D�� D�  D�>�D�~�D�� D�  D�@ D�� D��HD�  D�@ D�~�D���D�  D�>�D�~�D��HD�HD�>�D�� D�� D���D�@ D��HD�� D�HD�AHD�� D��qD���D�>�D�� D�� D�  D�>�D��HD�� D�  D�@ D�}qD���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD��HD���D�>�D��HD��HD�  D�>�D�� D�� D�  D�AHD�� D��HD��D�@ D�~�D���D�  D�AHD�� D���D���D�@ D��HD�� D�  D�>�D�� D��HD�  D�AHD��HD�� D���D�@ D��HD�� D�  D�@ D��HD��HD�  D�@ D�~�D���D�  D�AHD��HD�� D�  D�>�D�~�D���D���D�@ D�~�D���D�  D�@ D�~�D���D�HD�AHD�� D���D���D�@ D�� D���D�  D�@ D�~�D�� D�  D�@ D�� D�� D���D�@ D��HD�D�  D�@ D��HD�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�~�D���D�HD�AHD�� D�� D���D�>�D�~�D�� D�HD�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�@ D�� D�� D�HD�AHD��HD���D�  D�AHD�� D�� D�  D�>�D�� D�� D�  D�>�D�� D�� D�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�@ DHD�� D�  D�@ DÀ D��HD�HD�AHDĀ D�� D�  D�AHDŁHD��HD�  D�@ Dƀ Dƾ�D���D�@ Dǀ D�� D���D�@ DȁHD��HD�  D�@ DɁHD�� D�  D�@ D�~�DʽqD���D�@ DˁHD�� D�  D�AHD̀ D��HD�  D�@ D�~�D;�D�  D�@ D΁HD�� D�HD�AHDρHD�� D���D�>�D�}qDнqD�  D�@ D�~�DѾ�D���D�@ DҀ D�� D�  D�AHDӁHD�� D�  D�@ DԀ D��HD�HD�@ DՀ D��HD�  D�@ DցHD�� D���D�@ D׀ D׾�D���D�@ D؀ D��HD�  D�@ D�~�DٽqD���D�@ D�~�DڽqD�  D�@ D�~�D۾�D�HD�AHD܁HD�� D���D�>�D݀ D�� D�  D�@ DށHD�� D�  D�AHD߁HD��HD�HD�AHD��HD�� D�  D�@ D� D�� D�  D�>�D�~�D�� D�  D�AHD�HD�� D�  D�@ D� D��HD�HD�>�D�~�D�� D�  D�AHD� D澸D�  D�AHD� D羸D�  D�@ D� D�� D�  D�AHD� D�� D�  D�@ D� D�� D�HD�@ D� D��HD�HD�@ D�~�D�� D�  D�AHD� D���D�  D�@ D�HD��HD�HD�@ D� D�� D�  D�@ D�� D�D�HD�AHD� D�� D���D�@ D�HD��HD�  G�O�>�?.{?aG�?��R?Ǯ?�ff@�@
=@+�@:�H@Q�@aG�@z�H@��
@���@�
=@�G�@�=q@�33@�(�@�ff@У�@�Q�@�G�@�{@�
=A   A�A	��A\)Az�AQ�A�RA$z�A)��A.{A3�
A9��A>{AC33AH��AMp�AR�\AXQ�A\��Ab�\AhQ�Al��Aq�Aw�A|(�A���A�33A�A���A��A�{A�Q�A��A�A���A�33A�{A���A��A�{A���A�33A�{A�Q�A��HA��A�  A��\A��A��A\A���AǮA�=qA���AϮA�=qA���A�\)A��A���A߮A�=qA�z�A�
=A��A�z�A�
=A��A�z�A�\)A��A�z�A�\)B�B�\B�BG�B�RB�
B	G�B
�RB(�Bp�B�HB(�B��B�RBQ�B��B
=Bz�B�B
=B��B�B�B ��B"=qB#�B$��B&=qB'�
B)G�B*�\B,  B-G�B.�RB0(�B1p�B2�HB4Q�B5B7
=B8Q�B9B;\)B<��B=�B?\)B@��BB=qBC�BD��BFffBG�
BI�BJffBL  BMG�BN�HBP(�BQp�BR�HBTQ�BU�BW33BXz�BY�B[�B\��B^{B_�Ba�BbffBd  BeG�Bf�RBh(�Bi��Bj�HBlQ�Bm��Bo33Bpz�BqBs\)Bt��Bu�Bw\)Bx��By�B{�B|��B}�B�B�z�B�
=B��B�ffB�
=B�B�ffB��B��B�ffB�
=B��B�ffB��B��B�ffB�
=B�B�Q�B�
=B��B�Q�B���B��B�=qB�
=B���B�=qB���B��B�=qB��HB��B�=qB���B�p�B�(�B���B�p�B�  B��RB�\)B�  B���B�G�B��B��\B�33B��
B��\B�33B��
B�z�B��B��
B�ffB�
=B�B�Q�B���B���B�=qB���B��B�{B��RB�\)B��B��\B�33B��B�=qB���B�p�B��B�ffB���B�\)B�B�(�B��\B��HB�\)B�B�{B��\B���B�\)B�B�=qB���B���B�p�B��
B�(�B���B�
=B�\)B��
B�=qB���B�
=B�p�B��
B�Q�B���B�
=B�p�B�B�Q�B���B���B�\)B��
B�=qB��\B�
=B�\)B�B�(�B£�B���B�G�B�B�(�Bď\B��HB�G�BŮB�{BƏ\B��HB�G�BǙ�B�{B�ffB���B�33Bə�B�  B�ffB���B��B˙�B��B�Q�B̸RB��B�p�B��B�=qBΣ�B�
=B�p�B��B�(�BЏ\B�
=B�\)B�B�{B�z�B��HB�G�Bә�B�{B�ffB���B�33BՅB��B�=qB֣�B�
=B�\)B�B�{B�z�B���B�33Bٙ�B�  B�Q�Bڣ�B�
=B�\)B�B�(�B�z�B���B�33Bݙ�B��B�=qBޣ�B���B�\)B߮B�(�B��B���B�\)B��
B�=qB�RB�
=B�B�  B�z�B��HB�\)B��B�=qB�RB�G�B�B�=qB�\B�
=B�B�  B�z�B���B�p�B��B�ffB���B�\)B��B�Q�B���B�\)B��B�ffB��HB�\)B��
B�ffB��HB�p�B��B�z�B���B�p�B�  B�z�B�
=B��B�  B��\B�
=B���B�(�B��RB�G�B��
B�Q�B��HB��B�  B��\B��B�C (�C p�C �C ��CG�C�\C�
C�CffC�C�C33Cz�CC
=CQ�C��C�
C�CffC�C��C=qC�C��C{C\)C�C��C=qC�\C��C	(�C	p�C	C

=C
Q�C
��C
�C33C�CC{CQ�C��C�C=qC�\C�
C�Cp�C�C  CG�C��C�HC(�Cz�CC{C\)C��C�C33C�C��C{C\)C�C��C=qC�\C��C(�Cp�C�RC
=CQ�C��C�HC33Cp�C��C{C\)C�C�C=qC�C��C�C\)C�C��CG�C��C�HC(�Cz�CC
=C\)C��C��C=qC�\C�HC(�Cp�C�RC 
=C \)C �C ��C!G�C!�\C!�HC"(�C"p�C"C#{C#\)C#��C#��C$=qC$�\C$�
C%(�C%p�C%�RC&  C&Q�C&�\C&�
C'{C'\)C'��C'�HC(�C(\)C(�C(C)
=C)33C)p�C)��C)�
C*
=C*G�C*z�C*�C*�HC+�C+Q�C+�\C+C,  C,33C,\)C,��C,�
C-{C-G�C-z�C-�RC-�C.�C.Q�C.�\C.��C/  C/33C/ffC/��C/�
C0
=C0=qC0z�C0�C0�HC1�C1Q�C1�C1C1�C2(�C2\)C2�\C2�
C3
=C3G�C3z�C3�C3�HC4�C4\)C4�\C4��C5  C5=qC5p�C5�C5�C6�C6\)C6��C6��C7
=C7=qC7z�C7�C7�C8�C8\)C8��C8��C9
=C9=qC9�C9�RC9��C:33C:p�C:�C:�HC;(�C;\)C;�\C;��C<
=C<G�C<�C<�RC<��C=33C=ffC=��C=�HC>�C>\)C>�\C>��C?  C?=qC?p�C?�RC?��C@(�C@\)C@��C@�HCA{CAQ�CA�\CACB  CB=qCB�CB�RCB�CC(�CCffCC��CC�HCD{CDQ�CD��CD�
CE
=CEG�CEz�CECE��CF33CFz�CF�RCF��CG(�CGffCG�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                     @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�7LA�7LA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�A�A�?}A�?}A�=qA�?}A�?}A�?}A�A�A�A�A�A�A�C�A�C�A�A�A�A�A�?}A�7LA�+A�$�A�"�A� �A� �A�"�A�"�A��A��A��A�{A�VA�
=A�1A�%A�A���A���A��/A���Aǟ�AčPA�=qA��/A��\A�
=A�ƨA���A��^A�+A�ƨA���A���A�bNA�|�A�{A�l�A��A��A� �A��RA�A�t�A�K�A��jA���A�VA���A�dZA��`A�9XA�A�n�A�  A�Q�A�hsA�bNA���A���A��A�K�A��A�ƨA�hsA�  A�=qA���A�VA�9XA��hA�l�A���A�"�A�/A�O�A�33A�x�A��7A��yA�ȴA�+A��uA�oA�5?A�(�A�ĜA���AO�A}dZA{�AzbNAxĜAs�wAq��An~�Al�RAi��Af�RAe�Ad�jAbĜA`ĜA^�!AZ��AX��AV��ASAQ�FAQAOt�AN$�AL��AK�
AJ�AGl�AFE�AE�hAD�AB��A@E�A>��A>��A>-A<n�A<{A;��A;�A:��A9�#A9+A6r�A4�jA2�HA1��A1p�A0�`A0�A0A/�A.�9A.�DA-�
A,v�A)�A)hsA)7LA(��A'+A%�A$�+A#�mA#|�A"(�A!+A �A�wA��AoA^5A7LAhsAn�A|�A��A  A�-A?}A�A�AĜAƨAhsA
=A��AJA�-At�A�!AdZA
�HA
z�A
bA	�FA	\)A�AM�A�A%AM�A��AhsA�yA-Ap�A �@��w@��y@��#@���@��j@��w@�dZ@�o@��T@�O�@�&�@���@��7@��
@�o@�hs@��
@�"�@�=q@�Ĝ@�C�@�n�@��@�x�@�%@��`@�@��m@�@�7@��`@�1@���@� �@�33@�ff@ٺ^@��/@�9X@�"�@պ^@��/@Լj@�Q�@ҟ�@Ѓ@���@�C�@��@���@ə�@ȴ9@�v�@ź^@ċD@��#@���@�Ĝ@��9@��9@��@���@�z�@�9X@�9X@�A�@�(�@���@��
@��w@��F@�33@��D@�dZ@�dZ@��F@�E�@�?}@���@�1'@��@�S�@��!@�V@�=q@�@���@��@�r�@���@��
@���@�/@�%@��@��@��;@�\)@���@�^5@���@�?}@��/@�r�@�ƨ@��@��w@�+@���@�=q@��-@��`@�A�@�|�@�S�@�C�@�@��^@�G�@���@�j@�ƨ@���@�K�@��R@��#@�x�@�z�@��@�33@���@�-@��@�?}@��@��@��`@�Q�@�A�@�1'@��@��m@�ƨ@��F@�\)@���@�n�@��@�@�?}@�&�@��@���@�9X@�
=@��y@���@���@��\@�$�@�@�/@���@�Ĝ@���@�I�@��@��m@��@��P@�t�@��P@�|�@�dZ@�\)@�\)@�dZ@�33@��@�{@�p�@��u@�Z@���@�S�@�33@�
=@�^5@�E�@�{@���@��^@��^@��-@��h@���@���@���@���@��@�p�@�G�@�Ĝ@�bN@�I�@�I�@�9X@�1@��P@�+@�33@�;d@�;d@�+@��@���@���@�~�@�J@���@�O�@���@�(�@�b@��@���@�dZ@�33@��H@��H@�ȴ@��\@�ff@�M�@�$�@��@��@��T@��T@��#@���@�@���@���@��@��@���@���@�Q�@��@�1@��;@��@��y@��\@���@��@��@��@���@�J@��T@���@�`B@���@�j@�9X@�1@��@l�@~�@~��@~�+@~ff@~V@~V@~$�@}�@}��@}�-@}`B@|�/@{t�@{C�@{o@z�H@z�!@z^5@yx�@y&�@y�@x��@x��@x��@x�9@x�u@x1'@w�@w�w@w\)@v�y@v�+@v@u�h@u/@tz�@tI�@t�@t1@s��@s��@so@r=q@q�^@q�7@q�7@qhs@q&�@p��@o�;@o|�@o\)@o+@n��@n�y@nȴ@n�R@nv�@nV@n$�@m@m�-@m��@mO�@mV@l9X@j�H@j��@j��@j�\@j~�@j�@i��@ix�@ihs@iX@i�@h��@h  @g�@g�P@f�@fE�@f@e��@e�h@ep�@d�@c��@ct�@cdZ@cS�@c@b��@b�\@b^5@bM�@b�@a��@a�^@a�@`��@`�@`A�@_��@_�@_��@_�P@_|�@_+@^v�@]@]�@]O�@\�j@\(�@[��@[ƨ@[��@[t�@[t�@[dZ@[S�@[o@Z�!@Y��@Y�7@Yhs@Y7L@X��@X�@XbN@Xb@W�w@WK�@V��@V��@Vv�@VV@VE�@V@U�@U/@T�D@T9X@S��@SC�@R��@Rn�@R�@Q�^@Q�7@QX@Q7L@P��@O�@O�w@O��@O|�@O;d@N��@N@M�T@M�@L��@L��@L�D@Lj@LI�@K��@K��@K33@J�@J��@J-@I�^@I7L@H�9@HbN@H1'@H  @G�w@G\)@F��@FE�@F5?@F@E��@Ep�@E?}@D�/@Dz�@Dj@D(�@D1@Cƨ@C��@Ct�@CdZ@CS�@C33@Co@B�@B��@B�\@A�@A�7@AX@A�@@�u@?�@?��@?�@?|�@?\)@?\)@?K�@?K�@?;d@?
=@>$�@<�/@;�
@;t�@;S�@:�!@:^5@:�@9�#@9��@8��@8�@8b@7\)@7�@6�y@6ff@6{@5��@5�@5O�@5�@4��@4z�@4�@41@41@3��@3�@2�H@2�!@2��@2��@2��@2~�@2^5@2M�@2M�@2�@2J@1�@1��@1x�@0��@0A�@/�w@/l�@/+@.��@.��@.�y@.��@.@-@-�@-p�@-p�@-�h@-p�@-p�@-`B@,��@,Z@,Z@,(�@,�@,1@+�
@+��@+t�@+dZ@+33@+o@+@*�H@*��@*��@*�!@*�!@*�\@*~�@*�@)hs@)7L@(A�@(A�@(Q�@(1'@(1'@'�@'��@'��@'��@'�@'�P@'l�@'K�@&�R@%�T@%�-@%`B@%/@%�@%�@%V@%V@$�j@$I�@#�m@#ƨ@#�F@#�F@#��@#��@#�@#33@"�!@"=q@!��@!��@!&�@ �9@ �@ r�@ Q�@  �@��@�w@K�@ȴ@E�@�T@@@��@�h@`B@�@��@�/@�j@��@I�@�m@�F@��@�@�@dZ@33@@�!@n�@n�@=q@��@�^@��@�7@hs@7L@��@Q�@|�@l�@\)@K�@+@�R@E�@�@�h@p�@O�@�@��@��@�@�@�@�@�@��@j@�@1@�m@�F@S�@@@�H@^5@J@�#@��@x�@G�@%@�`@��@Ĝ@��@�w@+@
=@ȴ@�+@v�@ff@v�@ff@V@$�@@{@{@{@�@��@��@�@p�@p�@`B@O�@��@�/@��@��@�j@�@z�@Z@9X@�F@C�@"�@o@@
�H@
n�@
J@	��@	��@	��@	��@	��@	�7@	&�@	�@r�@1'@  A�5?A�7LA�5?A�5?A�9XA�;dA�;dA�7LA�7LA�5?A�5?A�5?A�9XA�;dA�9XA�9XA�7LA�9XA�9XA�=qA�;dA�;dA�9XA�9XA�9XA�;dA�;dA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�9XA�9XA�9XA�;dA�;dA�=qA�?}A�=qA�=qA�9XA�9XA�9XA�;dA�=qA�?}A�=qA�=qA�9XA�9XA�9XA�9XA�;dA�=qA�?}A�?}A�=qA�;dA�9XA�;dA�=qA�?}A�;dA�9XA�9XA�;dA�=qA�?}A�=qA�;dA�9XA�=qA�?}A�?}A�;dA�9XA�;dA�?}A�?}A�?}A�=qA�;dA�;dA�=qA�=qA�?}A�A�A�=qA�;dA�;dA�;dA�;dA�=qA�=qA�?}A�?}A�?}A�A�A�A�A�A�A�=qA�=qA�=qA�=qA�=qA�?}A�?}A�A�A�A�A�=qA�;dA�?}A�A�A�C�A�A�A�?}A�?}A�?}A�?}A�A�A�A�A�?}A�=qA�;dA�;dA�?}A�A�A�?}A�;dA�;dA�?}A�?}A�?}A�;dA�?}A�A�A�=qA�=qA�=qA�A�A�=qA�;dA�=qA�A�A�A�A�=qA�=qA�?}A�A�A�?}A�=qA�?}A�A�A�A�A�?}A�=qA�?}A�A�A�C�A�A�A�?}A�A�A�C�A�A�A�?}A�C�A�C�A�C�A�A�A�A�A�C�A�E�A�C�A�A�A�A�A�E�A�E�A�C�A�?}A�A�A�E�A�C�A�?}A�?}A�C�A�E�A�A�A�?}A�A�A�C�A�?}A�=qA�?}A�A�A�A�A�A�A�;dA�9XA�;dA�;dA�=qA�9XA�33A�1'A�-A�(�A�+A�-A�-A�(�A�$�A�&�A�&�A�$�A�"�A� �A� �A�"�A�$�A�"�A� �A� �A�"�A�$�A�"�A�$�A��A��A��A��A� �A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�"�A� �A�"�A� �A� �A� �A� �A�"�A�$�A�$�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�{A�{A�oA�bA�oA�oA�{A�{A�oA�bA�VA�VA�JA�VA�VA�JA�VA�JA�
=A�1A�1A�JA�JA�1A�%A�1A�
=A�JA�
=A�1A�%A�1A�
=A�1A�%A�A�1A�1A�1A�%A�A�%A�1A�1A�%A�A�A�%A�1A�%A�A�  A�A�A�A�A�A�  A�  A�A�A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��yA��TA��HA��HA��HA��;A��#A��#A��#A��#A��#A��
A���A���A�ȴA�ȴA�ĜAǼjA���AǸRAǴ9Aǰ!Aǰ!AǬAǰ!AǮAǩ�Aǩ�Aǧ�Aǝ�AǙ�AǓuAǁA�l�A�A�A�oAƥ�A��Aď\A���A�VA´9A�I�A��A�jA�n�A���A�M�A��A�~�A�VA�=qA�{A��#A���A�dZA��A��RA�^5A�(�A�
=A���A��A��`A���A��-A��7A�l�A��A��-A�x�A�XA�7LA�$�A���A���A��uA�|�A�`BA�M�A��A�ĜA��A�I�A�oA���A���A�v�A�"�A��RA�XA���A���A��RA��uA�E�A���A��uA�5?A��A���A�n�A�I�A�33A��A�oA�A��A��#A���A�A�A���A��^A��!A��!A���A���A���A���A��hA��\A��A�n�A�XA��HA��A�S�A�%A�A���A��A�\)A�A�A�&�A�1A��mA���A�~�A�dZA�XA�I�A�1'A�&�A�&�A��A��A�VA�  A��A��A��A��;A�^5A��A�A���A���A��hA��+A��A�|�A�ffA�M�A�=qA�(�A�1A��TA��jA�|�A�hsA�\)A�5?A��A�VA�1A��A�|�A�A��
A���A��+A�p�A�M�A�7LA�"�A�VA��A��HA��;A��A���A�ȴA��jA���A�K�A��A���A�x�A�ZA�M�A�C�A�7LA�$�A�  A��A��
A�A���A��+A�~�A�\)A�E�A�oA��A���A���A��hA�~�A�l�A�`BA�Q�A�A�A�/A�oA�%A���A���A���A��A� �A��A�|�A�ffA�M�A�G�A�A�A�A�A�=qA�&�A�-A��A���A��FA��!A���A���A���A�v�A�Q�A�I�A�1'A�(�A�1A��A��HA��9A���A��A�  A��wA��^A��9A��FA��-A���A���A���A���A��\A�VA�I�A�?}A�;dA�+A�-A�"�A��A�bA�1A���A��HA���A��wA���A�z�A�l�A�\)A�S�A�I�A�=qA�$�A���A���A��^A���A��7A�l�A�S�A�M�A�33A���A��TA���A��\A�`BA�E�A�A�A�C�A�=qA�bA�A�  A���A�  A���A���A��A��A��mA��
A��RA��jA���A���A���A���A���A���A���A��PA��A�~�A�~�A�z�A�v�A�r�A�p�A�n�A�l�A�dZA�\)A�Q�A�;dA�(�A�{A�%A��A��A���A�ƨA���A��A�p�A�\)A�A�A�oA��HA���A�ƨA��wA��A��A�(�A��A�ƨA���A�z�A�`BA�-A��;A���A�S�A�9XA�+A��A���A��TA��A���A���A�C�A���A��uA�=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                     A�9XA�7LA�7LA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�;dA�=qA�=qA�=qA�=qA�?}A�?}A�?}A�A�A�?}A�?}A�=qA�?}A�?}A�?}A�A�A�A�A�A�A�C�A�C�A�A�A�A�A�?}A�7LA�+A�$�A�"�A� �A� �A�"�A�"�A��A��A��A�{A�VA�
=A�1A�%A�A���A���A��/A���Aǟ�AčPA�=qA��/A��\A�
=A�ƨA���A��^A�+A�ƨA���A���A�bNA�|�A�{A�l�A��A��A� �A��RA�A�t�A�K�A��jA���A�VA���A�dZA��`A�9XA�A�n�A�  A�Q�A�hsA�bNA���A���A��A�K�A��A�ƨA�hsA�  A�=qA���A�VA�9XA��hA�l�A���A�"�A�/A�O�A�33A�x�A��7A��yA�ȴA�+A��uA�oA�5?A�(�A�ĜA���AO�A}dZA{�AzbNAxĜAs�wAq��An~�Al�RAi��Af�RAe�Ad�jAbĜA`ĜA^�!AZ��AX��AV��ASAQ�FAQAOt�AN$�AL��AK�
AJ�AGl�AFE�AE�hAD�AB��A@E�A>��A>��A>-A<n�A<{A;��A;�A:��A9�#A9+A6r�A4�jA2�HA1��A1p�A0�`A0�A0A/�A.�9A.�DA-�
A,v�A)�A)hsA)7LA(��A'+A%�A$�+A#�mA#|�A"(�A!+A �A�wA��AoA^5A7LAhsAn�A|�A��A  A�-A?}A�A�AĜAƨAhsA
=A��AJA�-At�A�!AdZA
�HA
z�A
bA	�FA	\)A�AM�A�A%AM�A��AhsA�yA-Ap�A �@��w@��y@��#@���@��j@��w@�dZ@�o@��T@�O�@�&�@���@��7@��
@�o@�hs@��
@�"�@�=q@�Ĝ@�C�@�n�@��@�x�@�%@��`@�@��m@�@�7@��`@�1@���@� �@�33@�ff@ٺ^@��/@�9X@�"�@պ^@��/@Լj@�Q�@ҟ�@Ѓ@���@�C�@��@���@ə�@ȴ9@�v�@ź^@ċD@��#@���@�Ĝ@��9@��9@��@���@�z�@�9X@�9X@�A�@�(�@���@��
@��w@��F@�33@��D@�dZ@�dZ@��F@�E�@�?}@���@�1'@��@�S�@��!@�V@�=q@�@���@��@�r�@���@��
@���@�/@�%@��@��@��;@�\)@���@�^5@���@�?}@��/@�r�@�ƨ@��@��w@�+@���@�=q@��-@��`@�A�@�|�@�S�@�C�@�@��^@�G�@���@�j@�ƨ@���@�K�@��R@��#@�x�@�z�@��@�33@���@�-@��@�?}@��@��@��`@�Q�@�A�@�1'@��@��m@�ƨ@��F@�\)@���@�n�@��@�@�?}@�&�@��@���@�9X@�
=@��y@���@���@��\@�$�@�@�/@���@�Ĝ@���@�I�@��@��m@��@��P@�t�@��P@�|�@�dZ@�\)@�\)@�dZ@�33@��@�{@�p�@��u@�Z@���@�S�@�33@�
=@�^5@�E�@�{@���@��^@��^@��-@��h@���@���@���@���@��@�p�@�G�@�Ĝ@�bN@�I�@�I�@�9X@�1@��P@�+@�33@�;d@�;d@�+@��@���@���@�~�@�J@���@�O�@���@�(�@�b@��@���@�dZ@�33@��H@��H@�ȴ@��\@�ff@�M�@�$�@��@��@��T@��T@��#@���@�@���@���@��@��@���@���@�Q�@��@�1@��;@��@��y@��\@���@��@��@��@���@�J@��T@���@�`B@���@�j@�9X@�1@��@l�@~�@~��@~�+@~ff@~V@~V@~$�@}�@}��@}�-@}`B@|�/@{t�@{C�@{o@z�H@z�!@z^5@yx�@y&�@y�@x��@x��@x��@x�9@x�u@x1'@w�@w�w@w\)@v�y@v�+@v@u�h@u/@tz�@tI�@t�@t1@s��@s��@so@r=q@q�^@q�7@q�7@qhs@q&�@p��@o�;@o|�@o\)@o+@n��@n�y@nȴ@n�R@nv�@nV@n$�@m@m�-@m��@mO�@mV@l9X@j�H@j��@j��@j�\@j~�@j�@i��@ix�@ihs@iX@i�@h��@h  @g�@g�P@f�@fE�@f@e��@e�h@ep�@d�@c��@ct�@cdZ@cS�@c@b��@b�\@b^5@bM�@b�@a��@a�^@a�@`��@`�@`A�@_��@_�@_��@_�P@_|�@_+@^v�@]@]�@]O�@\�j@\(�@[��@[ƨ@[��@[t�@[t�@[dZ@[S�@[o@Z�!@Y��@Y�7@Yhs@Y7L@X��@X�@XbN@Xb@W�w@WK�@V��@V��@Vv�@VV@VE�@V@U�@U/@T�D@T9X@S��@SC�@R��@Rn�@R�@Q�^@Q�7@QX@Q7L@P��@O�@O�w@O��@O|�@O;d@N��@N@M�T@M�@L��@L��@L�D@Lj@LI�@K��@K��@K33@J�@J��@J-@I�^@I7L@H�9@HbN@H1'@H  @G�w@G\)@F��@FE�@F5?@F@E��@Ep�@E?}@D�/@Dz�@Dj@D(�@D1@Cƨ@C��@Ct�@CdZ@CS�@C33@Co@B�@B��@B�\@A�@A�7@AX@A�@@�u@?�@?��@?�@?|�@?\)@?\)@?K�@?K�@?;d@?
=@>$�@<�/@;�
@;t�@;S�@:�!@:^5@:�@9�#@9��@8��@8�@8b@7\)@7�@6�y@6ff@6{@5��@5�@5O�@5�@4��@4z�@4�@41@41@3��@3�@2�H@2�!@2��@2��@2��@2~�@2^5@2M�@2M�@2�@2J@1�@1��@1x�@0��@0A�@/�w@/l�@/+@.��@.��@.�y@.��@.@-@-�@-p�@-p�@-�h@-p�@-p�@-`B@,��@,Z@,Z@,(�@,�@,1@+�
@+��@+t�@+dZ@+33@+o@+@*�H@*��@*��@*�!@*�!@*�\@*~�@*�@)hs@)7L@(A�@(A�@(Q�@(1'@(1'@'�@'��@'��@'��@'�@'�P@'l�@'K�@&�R@%�T@%�-@%`B@%/@%�@%�@%V@%V@$�j@$I�@#�m@#ƨ@#�F@#�F@#��@#��@#�@#33@"�!@"=q@!��@!��@!&�@ �9@ �@ r�@ Q�@  �@��@�w@K�@ȴ@E�@�T@@@��@�h@`B@�@��@�/@�j@��@I�@�m@�F@��@�@�@dZ@33@@�!@n�@n�@=q@��@�^@��@�7@hs@7L@��@Q�@|�@l�@\)@K�@+@�R@E�@�@�h@p�@O�@�@��@��@�@�@�@�@�@��@j@�@1@�m@�F@S�@@@�H@^5@J@�#@��@x�@G�@%@�`@��@Ĝ@��@�w@+@
=@ȴ@�+@v�@ff@v�@ff@V@$�@@{@{@{@�@��@��@�@p�@p�@`B@O�@��@�/@��@��@�j@�@z�@Z@9X@�F@C�@"�@o@@
�H@
n�@
J@	��@	��@	��@	��@	��@	�7@	&�@	�@r�@1'G�O�A�5?A�7LA�5?A�5?A�9XA�;dA�;dA�7LA�7LA�5?A�5?A�5?A�9XA�;dA�9XA�9XA�7LA�9XA�9XA�=qA�;dA�;dA�9XA�9XA�9XA�;dA�;dA�9XA�9XA�9XA�;dA�;dA�;dA�;dA�9XA�9XA�9XA�;dA�;dA�=qA�?}A�=qA�=qA�9XA�9XA�9XA�;dA�=qA�?}A�=qA�=qA�9XA�9XA�9XA�9XA�;dA�=qA�?}A�?}A�=qA�;dA�9XA�;dA�=qA�?}A�;dA�9XA�9XA�;dA�=qA�?}A�=qA�;dA�9XA�=qA�?}A�?}A�;dA�9XA�;dA�?}A�?}A�?}A�=qA�;dA�;dA�=qA�=qA�?}A�A�A�=qA�;dA�;dA�;dA�;dA�=qA�=qA�?}A�?}A�?}A�A�A�A�A�A�A�=qA�=qA�=qA�=qA�=qA�?}A�?}A�A�A�A�A�=qA�;dA�?}A�A�A�C�A�A�A�?}A�?}A�?}A�?}A�A�A�A�A�?}A�=qA�;dA�;dA�?}A�A�A�?}A�;dA�;dA�?}A�?}A�?}A�;dA�?}A�A�A�=qA�=qA�=qA�A�A�=qA�;dA�=qA�A�A�A�A�=qA�=qA�?}A�A�A�?}A�=qA�?}A�A�A�A�A�?}A�=qA�?}A�A�A�C�A�A�A�?}A�A�A�C�A�A�A�?}A�C�A�C�A�C�A�A�A�A�A�C�A�E�A�C�A�A�A�A�A�E�A�E�A�C�A�?}A�A�A�E�A�C�A�?}A�?}A�C�A�E�A�A�A�?}A�A�A�C�A�?}A�=qA�?}A�A�A�A�A�A�A�;dA�9XA�;dA�;dA�=qA�9XA�33A�1'A�-A�(�A�+A�-A�-A�(�A�$�A�&�A�&�A�$�A�"�A� �A� �A�"�A�$�A�"�A� �A� �A�"�A�$�A�"�A�$�A��A��A��A��A� �A�"�A�$�A�$�A�$�A�$�A�$�A�$�A�"�A� �A�"�A� �A� �A� �A� �A�"�A�$�A�$�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�{A�{A�oA�bA�oA�oA�{A�{A�oA�bA�VA�VA�JA�VA�VA�JA�VA�JA�
=A�1A�1A�JA�JA�1A�%A�1A�
=A�JA�
=A�1A�%A�1A�
=A�1A�%A�A�1A�1A�1A�%A�A�%A�1A�1A�%A�A�A�%A�1A�%A�A�  A�A�A�A�A�A�  A�  A�A�A�  A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��yA��TA��HA��HA��HA��;A��#A��#A��#A��#A��#A��
A���A���A�ȴA�ȴA�ĜAǼjA���AǸRAǴ9Aǰ!Aǰ!AǬAǰ!AǮAǩ�Aǩ�Aǧ�Aǝ�AǙ�AǓuAǁA�l�A�A�A�oAƥ�A��Aď\A���A�VA´9A�I�A��A�jA�n�A���A�M�A��A�~�A�VA�=qA�{A��#A���A�dZA��A��RA�^5A�(�A�
=A���A��A��`A���A��-A��7A�l�A��A��-A�x�A�XA�7LA�$�A���A���A��uA�|�A�`BA�M�A��A�ĜA��A�I�A�oA���A���A�v�A�"�A��RA�XA���A���A��RA��uA�E�A���A��uA�5?A��A���A�n�A�I�A�33A��A�oA�A��A��#A���A�A�A���A��^A��!A��!A���A���A���A���A��hA��\A��A�n�A�XA��HA��A�S�A�%A�A���A��A�\)A�A�A�&�A�1A��mA���A�~�A�dZA�XA�I�A�1'A�&�A�&�A��A��A�VA�  A��A��A��A��;A�^5A��A�A���A���A��hA��+A��A�|�A�ffA�M�A�=qA�(�A�1A��TA��jA�|�A�hsA�\)A�5?A��A�VA�1A��A�|�A�A��
A���A��+A�p�A�M�A�7LA�"�A�VA��A��HA��;A��A���A�ȴA��jA���A�K�A��A���A�x�A�ZA�M�A�C�A�7LA�$�A�  A��A��
A�A���A��+A�~�A�\)A�E�A�oA��A���A���A��hA�~�A�l�A�`BA�Q�A�A�A�/A�oA�%A���A���A���A��A� �A��A�|�A�ffA�M�A�G�A�A�A�A�A�=qA�&�A�-A��A���A��FA��!A���A���A���A�v�A�Q�A�I�A�1'A�(�A�1A��A��HA��9A���A��A�  A��wA��^A��9A��FA��-A���A���A���A���A��\A�VA�I�A�?}A�;dA�+A�-A�"�A��A�bA�1A���A��HA���A��wA���A�z�A�l�A�\)A�S�A�I�A�=qA�$�A���A���A��^A���A��7A�l�A�S�A�M�A�33A���A��TA���A��\A�`BA�E�A�A�A�C�A�=qA�bA�A�  A���A�  A���A���A��A��A��mA��
A��RA��jA���A���A���A���A���A���A���A��PA��A�~�A�~�A�z�A�v�A�r�A�p�A�n�A�l�A�dZA�\)A�Q�A�;dA�(�A�{A�%A��A��A���A�ƨA���A��A�p�A�\)A�A�A�oA��HA���A�ƨA��wA��A��A�(�A��A�ƨA���A�z�A�`BA�-A��;A���A�S�A�9XA�+A��A���A��TA��A���A���A�C�A���A��uA�=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                     ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�bB�-B�-B��B�-B�bB�-B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�bB��B��B��B��B��B��B��B�@B��B�LB�$B�qB��B��B��B��B�jB�OB�'B��B�BɆB��B˒B�0B̘B��B�B�B�dB��B��BƨB��B�_B��B�MB{�By�By�BxlBzDBs�Bo�Bl�Bq�BoiBp�Bm]BpoBj�Bm�BiyBl�Be�Be�BaHB_�B_BYBT�BS�BN�BI�BE�BA�B>�B<B5?B-wB&B#�B!bBBBPB�BݘB��B��B�6B�xB�B�=B�MB��Bv�Bf�B[WBMjBA�B)�B�BVB�B�B��B�KB��B�XB��B�7B�iBtTBjBOB<�B+6B=B:B
��B
�GB
�B
�/B
�}B
��B
�wB
��B
�B
�4B
v�B
pB
k�B
cTB
ZQB
U2B
P�B
C�B
<�B
8RB
4nB
/�B
+�B
%�B
#B
!�B
�B
�B
�B
_B
�B
:B

=B

=B	�(B	��B	�2B	�+B	�ZB	�B	�B	�;B	�/B	�WB	�KB	��B	�B	�B	یB	�WB	�B	�9B	�[B	бB	�HB	͟B	˒B	�RB	�mB	��B	��B	��B	�B	��B	��B	�$B	��B	�tB	��B	��B	�hB	�@B	��B	��B	�xB	��B	��B	�	B	��B	��B	��B	�$B	�MB	��B	�B	��B	�B	��B	�@B	�@B	��B	��B	��B	�PB	�DB	�xB	�lB	��B	�	B	�7B	�B	�7B	��B	��B	�"B	��B	��B	��B	��B	��B	�kB	�kB	��B	�!B	�!B	�OB	��B	�'B	�VB	��B	��B	�_B	�0B	�B	��B	��B	��B	��B	��B	��B	��B	�BB	ÖB	ŢB	��B	��B	�^B	͟B	��B	�}B	ϫB	��B	�pB	�0B	ǮB	B	�B	�B	�tB	��B	�dB	�pB	ѷB	�
B	�#B	ݘB	�pB	�HB	�NB	�NB	�B	�B	�ZB	�2B	�)B	�8B	��B	�(B	�.B
oB
B
�B
uB
\B
B
�B
_B
�B
�B
=B
�B
IB
�B
B
�B
�B
#�B
%B
$tB
(�B
*eB
)�B
,�B
/B
0�B
2�B
49B
4nB
4nB
6B
7�B
9�B
?}B
A B
A�B
FtB
G�B
J#B
L0B
OB
R�B
U�B
U�B
UgB
V�B
[�B
\�B
]�B
`B
c�B
f�B
h>B
lWB
p�B
rB
v`B
y�B
{B
}�B
cB
�;B
�oB
�B
�AB
�MB
��B
��B
��B
�+B
�1B
��B
�1B
�B
�~B
��B
��B
�(B
�bB
��B
��B
��B
��B
��B
�_B
��B
�7B
�	B
�IB
�OB
�4B
��B
��B
��B
�kB
�B
�=B
��B
�-B
��B
��B
�B
�XB
�B
�<B
�BB
�[B
�'B
�'B
��B
��B
�aB
ŢB
ȀB
ȀB
�RB
��B
�<B
��B
�BB
�B
�NB
��B
�,B
�gB
՛B
�9B
��B
�9B
֡B
�?B
ٴB
یB
��B
�WB
�)B
ݘB
�vB
��B
�B
��B
�2B
��B
�8B
�B
��B
�QB
��B
��B
�oB
�B
��B
�	B
��B
�B
��B
�PB
��B
��B
��B �BB�BuB�B�BB�B�BB�BSBBB�B	B
=BxBB~B�B�B�B�B�B�B�B�B�B@BB�BB$BeBeBkB=B�BBOB�B�B�B�B!B�B �B �B!�B"hB&�B&LB&�B'B'RB'�B)�B*eB*�B*�B*�B*�B+B+6B+�B,qB,qB-CB.IB.�B0!B0�B1�B33B3hB4B3�B3�B4�B5�B7B8�B8�B8�B8�B9XB9�B<B<�B<�B=B=<B=<B=�B=�B>B>BB>wB?}B?}B?B?�B@BA�BC�BC�BC�BC�BC�BD�BEmBEmBE�BE�BFBFBG�BG�BGzBIRBI�BI�BJ#BJ#BJXBK^BL�BMBL�BL�BMjBM�BNBNBNBNpBNpBOBPBPHBPBP�BQBQNBQNBQNBQBQ�BR�BS&BS[BS�BT�BT�BT�BU�BVBU�BVBU�BVBV9BV�BW�BXBXEBXBYKBX�BYBYBY�BZQBZ�BZ�BZ�B[#BZ�B[WB[�B\)B\�B\�B]/B^5B^�B^�B_;B_�B_�B_�B`B`BBbBa�Ba�Ba�Ba�Bc Bc BcTBc�Bd�Bd�Bd�Bd�Bd�Be�Bf2Bf�BgBgmBg�BhsBh�Bi�Bi�Bi�Bi�Bi�BjKBj�BjBjKBjBj�BkBkQBk�Bk�BlWBm�Bn/Bn�Bn�Bo Bo Bo5Bo5BoiBo�Bo�BpBp�BqvBq�BrBr�Bs�Bs�Bs�BtBtTBtTBtTBtTBtTBtTBu�Bv�BxBxBw�By	By	By	By�By�BzBzBz�B{�B{�B|B|�B|�B|�B}�B}�B}�B}�B.B�B�B�B�4B�B��B�B�;B�;B�oB�oB��B��B��B�B�B�B�B�B�uB��B��B�GB��B��B�{B�{B��B��B�uB�B�AB�AB�uB�uB�uB�AB��B��B��B��B��B��B��B�YB��B��B��B��B�+B�+B�+B�+B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�YB�%B��B�YB��B��B�YB�%B�YB�YB�%B�YB��B�%B��B�+B��B�+B�+B�+B�+B��B��B�_B��B�fB��B�B�lB�=B�	B�=B�rB��B��B��B�DB��B�JB��B��B��B��B��B�B�PB��B��B��B��B�"B��B��B��B��B��B��B�(B��B��B��B��B�bB��B� B��B�bB�bB��B��B�4B�B�:B�oB��B�oB�uB�uB��B�FB�FB�{B��B��B�B�MB�MB�MB�MB�MB�MB�B��B��B��B��B�YB�$B�$B��B��B��B��B��B�+B�_B��B��B�_B�+B�_B��B�7B�kB��B�=B�=B�=B�qB�qB�qB��B��B��B�qB�qB�=B��B��B�B�B�B�CB�B��B��B��B�B�B�IB�~B��B��B��B�VB��B��B��B��B�\B�-B�bB�bB��B��B��B�4B�hB�4B��B��B��B�bB��B��B�4B��B�\B�'B�-B��B��B��B�hB�'B�'B�\B��B��B��B�-B��B�'B�-B��B��B�-B��B��B��B��B�bB��B�'B��B�\B�hB��B�hB��B��B��B��B�bB��B��B�B��B��B��B��B��B�-B�hB��B��B�4B��B�bB��B��B��B��B�B��B��B�bB�-B�4B��B��B��B��B�bB��B��B��B��B��B��B��B��B��B��B��B�4B�hB��B��B��B��B��B�-B�4B��B��B�4B��B��B�-B��B�bB��B��B�-B�4B�4B�hB�hB�4B��B�bB�\B��B��B�hB��B�-B��B��B��B�hB�hB�-B�\B��B�-B�4B��B�4B�-B��B�bB�hB�4B�-B��B��B��B�-B��B�4B��B��B��B�hB��B��B��B�-B��B�hB��B�-B�4B�B��B�-B�bB��B��B��B��B��B�4B�:B��B�bB��B��B�nB�:B��B��B�tB��B��B�tB�FB�tB�B�B�@B��B�B��B�zB��B��B��B�FB�RB��B��B��B��B��B��B�0B�B��B��B�[B��B��B��B��B�LB��B��B�^B��B��B��B�*B��B��B��B�*B�0B�dB��B��B��B��B��B��B�0B�^B�B�dB��B�BB��B�}B�OB�B�OB��B��B� B�'B�'B��B�-B�[B�-B�aB�aBÖB��BBĜB��BĜB�9B�mB�?BƨB�zB�BɆB�#B�XB˒B��B˒B�^B˒B��BʌB�#B�XBʌB�^B�^B�)B˒B̘B��B˒B�^B��B��B�)B˒B��B�B�B�0B��B�0B˒B�dB�B��B��B˒B��B��B��B��B�dB˒B��B��B��B�6B��B̘B��B��B��B�0B��B�6B�<B��B�0B��B�B͟B͟B̘B��B�dB�6B�<B��B�0B��B��B̘B�B�jB̘B��B�0B�B�jB�dB�dB�^B�)B�0B��B�B˒B�)B��B̘B��B�)B��B��B�^B��BɆB�RBʌB�XBȀB��B�BɆB��B�RBȴB��B�EBȀB�B�3B�B��BB�aB�UB��B��B��B�wB�B�B�^B�*B�B��B��B�B�'B�9B�XB��B�IB�MB�%B�-B��B�B�iB�fB�lB�B~]B�%B�B��B�B��B��B��B�GB�;Bz�B{JB|B{�BxBzBsB�B�SB{BxBw�BuZByrB{B|�ByrBw�Bv�Bz�B}"B�Bw�Bv�Bt�ByrBw�B|�B{JBv+B}�Bo5BsBp�B|B}�BxB�oB{B��Bx8BtTBt�Bq�Bp�Bp�BrBr�BpBqBo5Bm]Bn�BoiBl�Bm]BncBk�Bl�BlWBkBi�BkQBk�BcBt�Bs�BtBpBqvBm�Bo�Bn/Bq�Bo5Bp�Bt�Bp;Bo5Bo�Bo BoiBo BlWBm)Bm�Bl�Bm�Bl"BjBi�Bl�B��Bo�BsMBncBjBjBk�BjBh�Bl�Bl�BkQBk�Bn�BncBo Bm�Bj�Bk�Bk�Bi�Bf�Bf2BiDB{BkQBpBj�BgBh�BiyBgBg�Bf�Bj�Be�Ba�BcTBa�BcTBb�Be`BqvBh>BiyBb�Be�B`vB_�B_pBaB^�B_;B`�B_;BbB]�B^5B`B^�B`B`BB`B^jB\�BYBYBZQBW
BW�BX�B/OBV9BT�BR�BZ�B[�B`vB`�BR�BR�BQ�BOvBO�BN<BN�BS�BI�BZ�BN�BMBHKBJ�BI�BNpBJ#BH�BH�BK�BC�BL�BH�BF�BI�BD�BH�B[�BE9BC-BA�B@�BA�BD�BB�BA B=qBA�BA�B@�BB[B?HB@�B>B?�B?}B=�B?�B@�B=qB?B=<B>BB?B:�B:^B:�B9$B9�B<6B@�B9$B8RB6FB4nB6�B2�B1'B4�B4nB1[B0UB0�B/B,qB)_B'�B+B*�B%FB$@B&LB$�B%FB'�B($B$tB$�B"4B#�B"hB%B(�B#:B!�B"�B"4B!�B!�B#nB!�B!bB �B �B�B 'B�BOB�B!BCB�B~B�B�BBB�BB�B@B�BB�BuB B�B�B�B�B	lB�B5�B�B�8B��B�B�%B��B�oB�]B�NB�5B�jB�;B�B�B�B�jB�B�HB��B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202102042107272021020421072720210204210727202102042107272021020421072720210204210727SI  SI  ARFMARFM                                                                                                                                                2021012422344120210124223441IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021020323005520210203230055QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021020323005520210203230055QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021020407312620210204073126IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021020421072820210204210728IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V3                        ARGO_for_DMQC Climatology Version 2020V3                        2021020421072820210204210728IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021020421072820210204210728IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                